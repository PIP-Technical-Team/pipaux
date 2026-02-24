# Testing script that respects dependency order for new releases

pipfun::setup_working_release(release = "20260101", identity = "TEST")
devtools::load_all()

# ============================================================================
# Build topological sort of dependency graph ####
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
# Run updates in topological order ####
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
# Summary report ####
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

# ============================================================================
# Test update_aux_measures with logging ####
# ============================================================================

test_that("update_aux_measures processes multiple measures in order with unified log", {
  
  # Select a subset of measures (choose non-error-prone ones for testing)
  test_measures <- c("cp", "metaregion")
  # "npl")
  
  # Get the aux_meta_alias for log retrieval later
  aux_meta_alias <- get_from_auxenv("aux_meta_alias")
    
  # Call update_aux_measures with logging enabled and save
  update_aux_measures(
    measures = test_measures,
    repo = NULL,
    owner = "RossanaTat",
    tag = NULL,
    log = TRUE,
    log_overwrite = TRUE,
    verbose = FALSE,
    halt_on_dep_fail = FALSE,
    log_save = TRUE
  )
  
  # Verify: Check that the unified log exists in memory
  expect_true(
    rlang::env_has(.piplogenv, "last_aux_log"),
    info = "Log should be stored in .piplogenv with the specified name"
  )
  
  # Retrieve the in-memory log name 
  unified_log <- aux_log_last()
  
  expect_s3_class(unified_log, "piplog")
  expect_s3_class(unified_log, "data.table")
  
  # Verify: Log contains entries for all processed measures
  expect_true(
    nrow(unified_log) > 0,
    info = "Log should contain entries for processed measures"
  )
  
  # Check that all test measures appear in the log metadata
  log_measures <- unique(sapply(unified_log$logmeta, function(x) x[["measure"]]))

  for (measure in test_measures) {
    expect_true(
      measure %in% log_measures,
      info = sprintf("Measure '%s' should appear in the unified log", measure)
    )
  }
  
  # Verify: Log contains expected step types
  expected_steps <- c("UPDATE_GH", "UPDATE_Y", "END")
  log_steps <- unique(sapply(unified_log$logmeta, function(x) x[["step"]]))

  # At least some of these should be present (depending on what actually updates)
  expect_true(
    any(expected_steps %in% log_steps),
    info = "Log should contain recognizable step types"
  )
  
  # Verify: No duplicate measure entries (each measure should be processed once)
  measure_counts <- table(sapply(unified_log$logmeta, function(x) x[["measure"]]))
  for (measure in test_measures) {
    if (measure %in% names(measure_counts)) {
      # Each measure should be processed, not duplicated excessively
      expect_lte(
        measure_counts[[measure]], 10
      )
    }
  }
  
})

test_that("update_aux_measures persists log to disk and can be retrieved", {
  
  aux_meta_alias <- get_from_auxenv("aux_meta_alias")
  test_measures <- c("cpi")
  
  #log_filename <- paste0("test_aux_persist_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  
  # Run update with log persistence
  update_aux_measures(
    measures = test_measures,
    repo = NULL,
    owner = "RossanaTat",
    log = TRUE,
    log_overwrite = TRUE,
    log_save = TRUE
  )
  
  # The log should now be saved to disk via the aux_meta_alias
  # Retrieve it using pipfun::log_load
  loaded_log <- pipfun::log_load(
    id = aux_log_last_name(), 
    alias = aux_meta_alias,
    verbose = FALSE,
    overwrite = TRUE
  )
  
  # Verify: Retrieved log is valid
  expect_s3_class(loaded_log, "piplog")
  expect_s3_class(loaded_log, "data.table")
  expect_true(nrow(loaded_log) > 0)
  
  # Verify: Log contains expected columns
  expected_cols <- c("event", "message", "logmeta")
  for (col in expected_cols) {
    expect_true(
      col %in% names(loaded_log),
      info = sprintf("Log should contain '%s' column", col)
    )
  }
  
})

test_that("update_aux_measures respects dependency order in log", {
  
  test_measures <- c("pfw", "npl")  # npl depends on pfw
  
  update_aux_measures(
    measures = test_measures,
    repo = NULL,
    owner = "RossanaTat",
    log = TRUE,
    log_save = FALSE
  )
  
  # Retrieve the log
  unified_log <- rlang::env_get(.piplogenv, aux_log_last_name())

  # Extract measure and step from logmeta list column
  unified_log[, c("measure", "step") := list(
    sapply(logmeta, function(x) x[["measure"]]),
    sapply(logmeta, function(x) x[["step"]])
  )]
  
  # Get first occurrence of each measure (where step == "END" marks completion)
  first_appearance <- unified_log[
    step == "END",
    .(first_row = min(.I)),
    by = "measure"
  ]
  
  # Verify: pfw should appear before npl (pfw is a dependency of npl)
  pfw_row <- first_appearance[measure == "pfw", first_row]
  npl_row <- first_appearance[measure == "npl", first_row]
  
  expect_true(
    pfw_row < npl_row,
    info = "pfw (dependency) should be processed before npl (dependent)"
  )
  
})

# ============================================================================
# Interactive testing ####
# ============================================================================

cat("\n", strrep("=", 80), "\n", sep = "")
cat("INTERACTIVE TEST: update_aux_measures with detailed inspection\n")
cat(strrep("=", 80), "\n\n")

# Select test measures (small subset for quick feedback)
test_measures <- c("cp", "metaregion", "npl", "pfw", "cpi", "pop")
aux_meta_alias <- get_from_auxenv("aux_meta_alias")

cat(sprintf("Running update_aux_measures for: %s\n", paste(test_measures, collapse = ", ")))

# Run the update
start_time <- Sys.time()
update_aux_measures(
  measures = test_measures,
  repo = NULL,
  owner = "RossanaTat",
  tag = NULL,
  log = TRUE,
  log_overwrite = TRUE,
  verbose = FALSE,
  halt_on_dep_fail = FALSE,
  log_save = TRUE
)
elapsed_total <- as.numeric(Sys.time() - start_time)

cat(sprintf("✓ Total execution time: %.2f seconds\n\n", elapsed_total))

# ============================================================================
## Inspect the in-memory log ####
# ============================================================================

cat(strrep("-", 80), "\n")
cat("IN-MEMORY LOG INSPECTION\n")
cat(strrep("-", 80), "\n\n")

# Retrieve the log from memory
unified_log <- rlang::env_get(.piplogenv, aux_log_last_name())

cat(sprintf("Log class: %s\n", paste(class(unified_log), collapse = ", ")))
cat(sprintf("Log dimensions: %d rows × %d columns\n", nrow(unified_log), ncol(unified_log)))
cat(sprintf("Columns: %s\n\n", paste(names(unified_log), collapse = ", ")))

# Event summary
cat("Event summary:\n")
event_summary <- unified_log[, .N, by = "event"]
print(event_summary)

cat("\n")

# Measure summary from logmeta - FIXED
cat("Measures processed (from logmeta):\n")
unified_log[, c("measure", "step") := list(
  sapply(logmeta, function(x) x[["measure"]]),
  sapply(logmeta, function(x) x[["step"]])
)]
measure_summary <- unified_log[
  !is.na(measure),
  .(n_entries = .N),
  by = "measure"
]
print(measure_summary)

cat("\n")

# Step types processed - FIXED
cat("Step types in log:\n")
step_summary <- unified_log[
  !is.na(step),
  .(n_entries = .N),
  by = "step"
]
print(step_summary)

cat("\n")

# Show any errors
cat(strrep("-", 80), "\n")
cat("ERROR ENTRIES\n")
cat(strrep("-", 80), "\n")

errors <- unified_log[event == "error"]
if (nrow(errors) > 0) {
  for (i in 1:nrow(errors)) {
    row <- errors[i]
    cat(sprintf("[%d] %s\n", i, row$message))
  }
} else {
  cat("✓ No errors detected\n")
}

cat("\n")

# ============================================================================
## Load and inspect persisted log from disk ####
# ============================================================================

cat(strrep("-", 80), "\n")
cat("PERSISTED LOG INSPECTION (from disk)\n")
cat(strrep("-", 80), "\n\n")

log_name_interactive <- aux_log_last_name()
cat(sprintf("Attempting to load persisted log: %s\n", log_name_interactive))
cat(sprintf("Using alias: %s\n\n", aux_meta_alias))

loaded_log <- tryCatch({
  pipfun::log_load(
    id = log_name_interactive,
    alias = aux_meta_alias,
    verbose = FALSE,
    overwrite = TRUE
  )
}, error = function(e) {
  cat(sprintf("✗ Failed to load persisted log: %s\n", e$message))
  return(NULL)
})

if (!is.null(loaded_log)) {
  cat(sprintf("✓ Successfully loaded persisted log\n"))
  cat(sprintf("  Dimensions: %d rows × %d columns\n", nrow(loaded_log), ncol(loaded_log)))
  cat(sprintf("  Row counts match: %s\n", nrow(unified_log) == nrow(loaded_log)))
} else {
  cat("⚠ Persisted log could not be loaded\n")
}

cat("\n")

# ============================================================================
## Compare in-memory vs. persisted logs ####
# ============================================================================

if (!is.null(loaded_log)) {
  cat(strrep("-", 80), "\n")
  cat("CONSISTENCY CHECK: In-Memory vs. Persisted\n")
  cat(strrep("-", 80), "\n\n")
  
  cat(sprintf("In-memory log nrows:  %d\n", nrow(unified_log)))
  cat(sprintf("Persisted log nrows:  %d\n", nrow(loaded_log)))
  
  if (nrow(unified_log) == nrow(loaded_log)) {
    cat("✓ Log persistence successful\n")
  } else {
    cat("✗ Row count mismatch between in-memory and persisted logs\n")
  }
}

cat("\n")
cat(strrep("=", 80), "\n")
cat("END OF INTERACTIVE TEST\n")
cat(strrep("=", 80), "\n\n")

# ============================================================================
# Simulate changes for multiple measures ####
# ============================================================================
# Setup
pipfun::setup_working_release(release = "20260101", identity = "TEST")
devtools::load_all()

cat("\n", strrep("=", 80), "\n", sep = "")
cat("SIMULATING CHANGES FOR AUXILIARY DATA\n")
cat(strrep("=", 80), "\n\n", sep = "")

# Define measures to simulate
measures_to_simulate <- c("cpi", "pop", "pfw", "gdp", "pce")

# Define simulation parameters for each measure
simulation_config <- list(
  cpi = list(drop = c(1, 5, 10), indices = c(20, 50, 100)),
  pop = list(drop = c(2, 8), indices = c(15, 45, 90)),
  pfw = list(drop = c(3, 7), indices = c(10, 30, 60)),
  gdp = list(drop = c(4, 9), indices = c(25, 55, 95)),
  pce = list(drop = c(1, 6, 11), indices = c(18, 48, 88))
)

# Track results
results <- data.table::data.table(
  measure = character(),
  status = character(),
  rows_dropped = integer(),
  rows_modified = integer(),
  elapsed_sec = numeric(),
  error_msg = character()
)

for (measure in measures_to_simulate) {
  
  cat(sprintf("\n[%s] Simulating changes...\n", measure))
  
  start_time <- Sys.time()
  status <- "success"
  error_msg <- NA_character_
  config <- simulation_config[[measure]]
  
  result <- tryCatch({
    
    simulate_old_release(
      measure = measure,
      seed = 123,
      drop = config$drop,
      indices = config$indices,
      verbose = TRUE
    )
    
  }, error = function(e) {
    status <<- "error"
    error_msg <<- e$message
    cat(sprintf("  ✗ ERROR: %s\n", e$message))
    return(NULL)
  })
  
  elapsed <- as.numeric(Sys.time() - start_time)
  
  if (status == "success") {
    cat(sprintf("  ✓ Simulation complete\n"))
    cat(sprintf("  Rows dropped: %d\n", length(config$drop)))
    cat(sprintf("  Rows modified: %d\n", length(config$indices)))
  }
  
  results <- rbind(results, data.table::data.table(
    measure = measure,
    status = status,
    rows_dropped = length(config$drop),
    rows_modified = length(config$indices),
    elapsed_sec = elapsed,
    error_msg = error_msg
  ))
  
  cat(sprintf("  ⏱ %.2f seconds\n", elapsed))
}

# ============================================================================
# Summary report ####
# ============================================================================

cat("\n", strrep("=", 80), "\n", sep = "")
cat("SIMULATION SUMMARY\n")
cat(strrep("=", 80), "\n\n", sep = "")

cat("Status breakdown:\n")
print(results[, .N, by = status])

if (nrow(results[status == "success"]) > 0) {
  cat("\nSuccessfully simulated measures:\n")
  print(results[status == "success", .(measure, rows_dropped, rows_modified, elapsed_sec)])
}

if (nrow(results[status == "error"]) > 0) {
  cat("\nFailed simulations:\n")
  print(results[status == "error", .(measure, error_msg)])
}

cat(sprintf("\nTotal time: %.2f seconds\n", results[, sum(elapsed_sec)]))

# ============================================================================
# Comparing changes ####
# ============================================================================

# # Switch to 20260202 as current release
# pipfun::setup_working_release(release = "20260202", identity = "TEST")
# # Switch to 20260101 as older release
# pipfun::setup_working_release(release = "20260101", identity = "TEST")


## Between diff releases ####
old_release <- "20260101_TEST"

# Part 1: focus on gdp, cpi, pfw, pop, pce and ppp
# Test on a single measure first to verify log structure, then expand to multiple measures

res_aux_rel = compare_aux_releases(measure = "cpi", old_release = old_release, owner = "RossanaTat")

## Between vintages (versions) within same release ####

res_aux_vint = compare_aux_vintages(measures = c("cpi", "pfw", "gdp", "pop", "pce", "ppp"))

# ============================================================================
# Diagnostics | Comparing changes ####
# ============================================================================

cat("\n", strrep("=", 80), "\n", sep = "")
cat("RELEASE COMPARISON DIAGNOSTICS\n")
cat(strrep("=", 80), "\n\n", sep = "")

run_release_comparison_diagnostics <- function(
  measures,
  old_release = NULL,
  owner = "RossanaTat"
) {

  results <- data.table::data.table(
    order           = integer(),
    measure         = character(),
    status          = character(),
    value_changes   = integer(),
    row_changes     = integer(),
    elapsed_sec     = numeric(),
    error_msg       = character()
  )

  for (i in seq_along(measures)) {

    m <- measures[i]

    cat(sprintf("\n[%d/%d] Comparing release: %s\n",
                i, length(measures), m))

    start_time <- Sys.time()
    status     <- "no_change"
    error_msg  <- NA_character_
    val_n      <- 0L
    row_n      <- 0L

    res <- tryCatch({

      get_aux_changes(
        measure     = m,
        old_release = old_release,
        verbose     = FALSE
      )

    }, error = function(e) {
      status <<- "error"
      error_msg <<- e$message
      cat(sprintf("  ✗ ERROR: %s\n", e$message))
      return(NULL)
    })

    if (!is.null(res)) {

      if (!is.null(res$diff_values))
        val_n <- nrow(res$diff_values)

      if (!is.null(res$diff_rows))
        row_n <- nrow(res$diff_rows)

      if (val_n > 0 & row_n > 0) {
        status <- "row_and_value_change"
      } else if (val_n > 0) {
        status <- "value_change"
      } else if (row_n > 0) {
        status <- "row_change"
      }
    }

    elapsed <- as.numeric(Sys.time() - start_time)

    results <- rbind(
      results,
      data.table::data.table(
        order         = i,
        measure       = m,
        status        = status,
        value_changes = val_n,
        row_changes   = row_n,
        elapsed_sec   = elapsed,
        error_msg     = error_msg
      )
    )

    cat(sprintf("  Status: %s\n", status))
    cat(sprintf("  Value diffs: %d\n", val_n))
    cat(sprintf("  Row diffs:   %d\n", row_n))
    cat(sprintf("  ⏱ %.2f seconds\n", elapsed))
  }

  return(results)
}

## RELEASE COMPARISON ####
# Example configuration
old_release <- "20260101_TEST"
test_measures <- c("cpi", "pfw", "gdp", "pop", "pce", "ppp")

release_diag <- run_release_comparison_diagnostics(
  measures    = test_measures,
  old_release = old_release
)

### Summary report ####
cat("\n", strrep("=", 80), "\n", sep = "")
cat("RELEASE COMPARISON SUMMARY\n")
cat(strrep("=", 80), "\n\n", sep = "")

cat("Status breakdown:\n")
print(release_diag[, .N, by = status])

changed <- release_diag[status != "no_change" & status != "error"]

if (nrow(changed) > 0) {
  cat("\nMeasures with detected changes:\n")
  print(changed[, .(measure, status, value_changes, row_changes)])
} else {
  cat("\n✓ No changes detected across measures\n")
}

cat("\nTiming summary:\n")
cat(sprintf("  Fastest: %s (%.2f s)\n",
            release_diag[which.min(elapsed_sec), measure],
            release_diag[, min(elapsed_sec)]))
cat(sprintf("  Slowest: %s (%.2f s)\n",
            release_diag[which.max(elapsed_sec), measure],
            release_diag[, max(elapsed_sec)]))
cat(sprintf("  Total: %.2f s\n",
            release_diag[, sum(elapsed_sec)]))

## VINTAGE COMPARISON DIAGNOSTICS ####
cat("\n", strrep("=", 80), "\n", sep = "")
cat("VINTAGE COMPARISON DIAGNOSTICS\n")
cat(strrep("=", 80), "\n\n", sep = "")

run_vintage_comparison_diagnostics <- function(
  measures,
  version = -1
) {

  results <- data.table::data.table(
    order           = integer(),
    measure         = character(),
    status          = character(),
    value_changes   = integer(),
    row_changes     = integer(),
    elapsed_sec     = numeric(),
    error_msg       = character()
  )

  for (i in seq_along(measures)) {

    m <- measures[i]

    cat(sprintf("\n[%d/%d] Comparing vintage: %s\n",
                i, length(measures), m))

    start_time <- Sys.time()
    status     <- "no_change"
    error_msg  <- NA_character_
    val_n      <- 0L
    row_n      <- 0L

    res <- tryCatch({

      compare_vintage_versions(
        measure = m,
        version = version,
        verbose = FALSE
      )

    }, error = function(e) {
      status <<- "error"
      error_msg <<- e$message
      cat(sprintf("  ✗ ERROR: %s\n", e$message))
      return(NULL)
    })

    if (!is.null(res)) {

      if (!is.null(res$diff_values))
        val_n <- nrow(res$diff_values)

      if (!is.null(res$diff_rows))
        row_n <- nrow(res$diff_rows)

      if (val_n > 0 & row_n > 0) {
        status <- "row_and_value_change"
      } else if (val_n > 0) {
        status <- "value_change"
      } else if (row_n > 0) {
        status <- "row_change"
      }
    }

    elapsed <- as.numeric(Sys.time() - start_time)

    results <- rbind(
      results,
      data.table::data.table(
        order         = i,
        measure       = m,
        status        = status,
        value_changes = val_n,
        row_changes   = row_n,
        elapsed_sec   = elapsed,
        error_msg     = error_msg
      )
    )

    cat(sprintf("  Status: %s\n", status))
    cat(sprintf("  Value diffs: %d\n", val_n))
    cat(sprintf("  Row diffs:   %d\n", row_n))
    cat(sprintf("  ⏱ %.2f seconds\n", elapsed))
  }

  return(results)
}

## Run Vintage Comparison Diagnostics ####
vintage_diag <- run_vintage_comparison_diagnostics(
  measures = test_measures,
  version  = -1
)

### Summary report ####
cat("\n", strrep("=", 80), "\n", sep = "")
cat("VINTAGE COMPARISON SUMMARY\n")
cat(strrep("=", 80), "\n\n", sep = "")

cat("Status breakdown:\n")
print(vintage_diag[, .N, by = status])

changed_vint <- vintage_diag[status != "no_change" & status != "error"]

if (nrow(changed_vint) > 0) {
  cat("\nMeasures with vintage changes:\n")
  print(changed_vint[, .(measure, status, value_changes, row_changes)])
} else {
  cat("\n✓ No vintage changes detected\n")
}

cat("\nTiming summary:\n")
cat(sprintf("  Total: %.2f s\n",
            vintage_diag[, sum(elapsed_sec)]))