# Testing script that respects dependency order for new releases

pipfun::setup_working_release(release = "20260101", identity = "TEST")
devtools::load_all()

# ============================================================================
# Build topological sort of dependency graph
# ============================================================================

build_topological_order <- function(deps_list) {
  visited <- new.env(parent = emptyenv())
  order <- character(0)
  
  visit <- function(measure) {
    if (rlang::env_has(visited, measure)) {
      return(invisible(NULL))
    }
    
    rlang::env_poke(visited, measure, TRUE)
    
    node_deps <- deps_list[[measure]]
    if (!is.null(node_deps) && length(node_deps) > 0) {
      for (dep in node_deps) {
        visit(dep)
      }
    }
    
    order <<- c(order, measure)
    invisible(NULL)
  }
  
  for (measure in names(deps_list)) {
    visit(measure)
  }
  
  order
}

# Load the dependency graph
deps <- read_dependencies(
  gh_user = "https://raw.githubusercontent.com",
  owner   = "PIP-Technical-Team"
)

# Build the processing order
ordered_measures <- build_topological_order(deps)

cat("Processing order (dependencies first):\n")
for (i in seq_along(ordered_measures)) {
  measure <- ordered_measures[i]
  measure_deps <- deps[[measure]]
  if (length(measure_deps) == 0) {
    cat(sprintf("%2d. %s (no deps)\n", i, measure))
  } else {
    cat(sprintf("%2d. %s (deps: %s)\n", i, measure, paste(measure_deps, collapse = ", ")))
  }
}

# ============================================================================
# Run updates in topological order
# ============================================================================

cat("\n", strrep("=", 80), "\n", sep = "")
cat("Starting ordered update run\n")
cat(strrep("=", 80), "\n\n", sep = "")

reset_logs <- function() {
  if (rlang::env_has(.piplogenv, "pipaux_update_log")) {
    rlang::env_delete(.piplogenv, "pipaux_update_log")
  }
}

reset_logs()

# Track results
results <- data.table::data.table(
  order = integer(),
  measure = character(),
  status = character(),
  elapsed_sec = numeric(),
  error_msg = character()
)

owner <- "RossanaTat"
processed_env <- new.env(parent = emptyenv())

# Get aux data alias for stamp::st_info()
aux_alias <- tryCatch({
  get_from_auxenv("aux_alias")
}, error = function(e) {
  NULL
})

for (i in seq_along(ordered_measures)) {
  measure <- ordered_measures[i]
  deps_for_measure <- deps[[measure]]
  
  cat(sprintf("\n[%2d/%d] Processing: %s", i, length(ordered_measures), measure))
  if (length(deps_for_measure) > 0) {
    cat(sprintf(" (deps: %s)", paste(deps_for_measure, collapse = ", ")))
  }
  cat("\n")
  
  start_time <- Sys.time()
  error_msg <- NA_character_
  status <- "success"
  
  tryCatch({
    aux_fun(
      measure       = measure,
      owner         = owner,
      processed     = processed_env,
      verbose       = FALSE,
      log           = TRUE,
      log_overwrite = FALSE
    )
  }, error = function(e) {
    error_text <- e$message
    
    # Classify certain errors as warnings instead of failures
    if (grepl("GitHub API error.*404|Branch not found", error_text)) {
      status <<- "warning"
      cat(sprintf("  ⚠ WARNING: %s\n", error_text))
    } else if (grepl("unused argument.*force", error_text)) {
      status <<- "error"
      error_msg <<- error_text
      cat(sprintf("  ✗ ERROR: %s\n", error_text))
    } else {
      status <<- "error"
      error_msg <<- error_text
      cat(sprintf("  ✗ ERROR: %s\n", error_text))
    }
  })
  
  if (status == "success") {
    # Try to load to verify
    tryCatch({
      artifact <- pipload::load_aux_data(measure = measure)
      
      cat(sprintf("  ✓ Updated and verified (%d rows)\n", nrow(artifact)))
      
      # Inspect artifact metadata via stamp::st_info()
      if (is.null(aux_alias)) {
        cat("    ℹ aux_alias not available, skipping stamp metadata inspection\n")
      } else {
        tryCatch({
          artifact_path <- paste0("aux_", measure, ".qs2")
          stamp_info <- stamp::st_info(artifact_path, alias = aux_alias)
          
          if (!is.null(stamp_info$sidecar)) {
            sidecar <- stamp_info$sidecar
            cat("    Sidecar metadata:\n")
            
            if (!is.null(sidecar$aux_key)) {
              cat(sprintf("      ✓ aux_key: %s\n", paste(sidecar$aux_key, collapse = ", ")))
            }
          
            if (!is.null(sidecar$gh)) {
              cat(sprintf("      ✓ gh metadata present\n"))
            }
          }
          
          if (!is.null(stamp_info$catalog)) {
            cat(sprintf("    Catalog: version %s (total: %d)\n", 
                       stamp_info$catalog$latest_version_id,
                       stamp_info$catalog$n_versions))
          }
          
          if (!is.na(stamp_info$snapshot_dir)) {
            cat(sprintf("    Snapshot: %s\n", basename(stamp_info$snapshot_dir)))
          }
        }, error = function(e) {
          cat(sprintf("    ℹ Could not inspect stamp metadata: %s\n", e$message))
        })
      }
    }, error = function(e) {
      error_text <- e$message
      
      # Check if this is a missing dependency issue
      if (grepl("does not exist", error_text)) {
        status <<- "warning"
        error_msg <<- paste("Dependency artifact missing:", error_text)
        cat(sprintf("  ⚠ WARNING: Dependency not yet available\n"))
      } else {
        status <<- "warning"
        error_msg <<- paste("Updated but failed to load:", error_text)
        cat(sprintf("  ⚠ Updated but failed to load: %s\n", error_text))
      }
    })
  }
  
  elapsed <- as.numeric(Sys.time() - start_time)
  
  results <- rbind(results, data.table::data.table(
    order = i,
    measure = measure,
    status = status,
    elapsed_sec = elapsed,
    error_msg = error_msg
  ))
  
  cat(sprintf("  ⏱ %.2f seconds\n", elapsed))
}

# ============================================================================
# Summary report
# ============================================================================

cat("\n", strrep("=", 80), "\n", sep = "")
cat("SUMMARY\n")
cat(strrep("=", 80), "\n\n", sep = "")

# Count by status
status_summary <- results[, .N, by = status]
cat("Status breakdown:\n")
print(status_summary)

# Show errors (not warnings)
errors <- results[status == "error"]
if (nrow(errors) > 0) {
  cat("\n", strrep("!", 80), "\n", sep = "")
  cat("ERRORS (critical failures):\n")
  cat(strrep("!", 80), "\n", sep = "")
  for (i in 1:nrow(errors)) {
    row <- errors[i]
    cat(sprintf("[%d] %s: %s\n", row$order, row$measure, row$error_msg))
  }
}

# Show warnings (non-critical)
warnings <- results[status == "warning"]
if (nrow(warnings) > 0) {
  cat("\n", strrep("-", 80), "\n", sep = "")
  cat("WARNINGS (non-critical):\n")
  cat(strrep("-", 80), "\n", sep = "")
  for (i in 1:nrow(warnings)) {
    row <- warnings[i]
    cat(sprintf("[%d] %s: %s\n", row$order, row$measure, row$error_msg))
  }
}

# Timing summary
cat("\nTiming summary:\n")
cat(sprintf("  Fastest: %s (%.2f s)\n", 
            results[which.min(elapsed_sec), measure],
            results[, min(elapsed_sec)]))
cat(sprintf("  Slowest: %s (%.2f s)\n", 
            results[which.max(elapsed_sec), measure],
            results[, max(elapsed_sec)]))
cat(sprintf("  Total: %.2f s\n", results[, sum(elapsed_sec)]))

# Final log
# cat("\n", strrep("=", 80), "\n", sep = "")
# cat("FINAL LOG\n")
# cat(strrep("=", 80), "\n\n", sep = "")

# final_log <- aux_log_last()

# if (!is.null(final_log)) {
#   cat(sprintf("Log has %d entries\n", nrow(final_log)))
#   cat("Event summary:\n")
#   event_summary <- final_log[, .N, by = event]
#   print(event_summary)
# } else {
#   cat("No log found\n")
# }

# # Display the results table
# cat("\n", strrep("=", 80), "\n", sep = "")
# cat("DETAILED RESULTS\n")
# cat(strrep("=", 80), "\n\n", sep = "")
# print(results)

# # Return for inspection
# invisible(results)