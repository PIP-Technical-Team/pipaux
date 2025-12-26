# Helper: Detect if top-level call
is_top_level <- function() {
  sys.nframe() <= 2
}

# Helper: Logging wrapper
log_event <- function(step, measure, event_type = "info", message = NULL, log = TRUE) {
  if (!log) return()
  if (is.null(message)) message <- paste0(step, " - ", measure)
  pipfun::log_add(
    event   = event_type,
    message = message,
    name    = "pipaux_update_log",
    logmeta = list(step = step, measure = measure)
  )
}

# Helper: Resolve repo and owner
resolve_measure_repo_owner <- function(measure, repo, owner) {
  repo  <- if (measure %in% c("income_groups", "country_list")) "Class" else repo
  owner <- if (measure == "nan") "PIP-Technical-Team" else owner
  list(repo = repo, owner = owner)
}

# Helper: Process dependencies recursively
process_dependencies <- function(measure, processed, owner, force, tag, verbose, log) {
  if (rlang::env_has(processed, measure)) return()
  
  rlang::env_poke(processed, measure, TRUE)
  
  dependencies_all <- read_dependencies(
    gh_user = "https://raw.githubusercontent.com",
    owner   = "PIP-Technical-Team"
  )
  
  deps <- dependencies_all[[measure]]
  if (is.null(deps)) deps <- character(0)
  
  for (dep in deps) {
    tryCatch(
      aux_fun(measure = dep,
              processed = processed,
              owner = owner,
              force = force,
              tag = tag,
              verbose = verbose,
              log = log),
      error = function(e) {
        log_event(step = "ERROR_DEP", measure = dep, event_type = "error",
                  message = paste0("Failed to process dependency ", dep, ": ", e$message),
                  log = log)
      }
    )
  }
}

# Helper: Execute update for GH and Y
execute_update <- function(measure, update_gh, update_y, release_branch, owner, repo, tag, force, verbose, log) {
  # GitHub update
  if (update_gh) {
    pipfun::sync_release_branch(
      owner         = owner,
      repo          = repo,
      ref_branch    = "DEV",
      target_branch = release_branch,
      verbose       = verbose
    )
    log_event(step = "UPDATE_GH", measure = measure,
              message = paste0("Updated GitHub for: ", measure),
              log = log)
  }
  
  # Y drive update
  if (update_y) {
    func_name <- paste0("aux_", measure)
    if (!exists(func_name, envir = asNamespace("pipaux"))) {
      cli::cli_abort(paste0("Function '", func_name, "' does not exist"))
    }
    
    func <- get(func_name, envir = asNamespace("pipaux"))
    all_args <- list(action = "update", branch = release_branch, force = force,
                     owner = owner, tag = tag, repo = repo)
    
    filtered_args <- all_args[names(all_args) %in% names(formals(func))]
    
    tryCatch({
      do.call(func, filtered_args)
      log_event(step = "UPDATE_Y", measure = measure,
                message = paste0("Updated Y drive for: ", measure),
                log = log)
    }, error = function(e) {
      log_event(step = "ERROR_Y", measure = measure, event_type = "error",
                message = paste0("Error updating Y drive: ", e$message),
                log = log)
      stop(e)
    })
  }
}

# Main aux_fun function
aux_fun <- function(measure,
                    repo      = paste0("aux_", measure),
                    owner     = getOption("pipfun.ghowner"),
                    processed = new.env(parent = emptyenv()),
                    force     = FALSE,
                    tag       = NULL,
                    log       = TRUE,
                    log_overwrite = TRUE,
                    verbose   = FALSE) {
  
  # Working release
  wrk_release <- get_from_auxenv(key = "wrk_release")
  release <- wrk_release$release
  identity <- wrk_release$identity
  release_branch <- paste0(release, "_", identity)
  
  if (is.null(tag)) tag <- release_branch
  
  # Initialize log at top level
  if (is_top_level() && log) {
    log_exists <- rlang::env_has(.piplogenv, "pipaux_update_log")
    if (log_overwrite || !log_exists) pipfun::log_init("pipaux_update_log", overwrite = log_overwrite)
  }
  
  # Resolve special repo/owner
  ro <- resolve_measure_repo_owner(measure, repo, owner)
  repo <- ro$repo
  owner <- ro$owner
  
  # Process dependencies
  process_dependencies(measure, processed, owner, force, tag, verbose, log)
  
  # Check update status
  check_result <- tryCatch(
    get_fs_status(measure = measure, repo = repo, owner = owner),
    error = function(e) {
      log_event(step = "ERROR_CHECK", measure = measure, event_type = "error",
                message = paste0("Check failed: ", e$message),
                log = log)
      NULL
    }
  )
  
  if (is.null(check_result)) return(invisible(NULL))
  
  update_gh <- check_result$update_gh
  update_y  <- check_result$update_y
  
  # Early return if nothing to update
  if (!update_gh && !update_y) {
    log_event(step = "END", measure = measure,
              message = paste0("No update needed for: ", measure),
              log = log)
    cli::cli_alert_success("No updates needed")
    return(invisible(NULL))
  }
  
  # Execute updates
  execute_update(measure, update_gh, update_y, release_branch, owner, repo, tag, force, verbose, log)
  
  # Final log
  if (is_top_level() && log) {
    log_event(step = "END", measure = measure,
              message = "Measure and dependencies successfully updated",
              log = log)
    cli::cli_alert_success(
      paste0("Log available: ", cli::bg_br_cyan(cli::col_black("{.strong pipaux_update_log}")),
             "\nUse {.code pipfun::log_get()} to access it")
    )
  }
  
  invisible(NULL)
}
