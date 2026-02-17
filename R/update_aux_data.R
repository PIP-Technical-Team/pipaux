#' Detect if the current call is top-level
#'
#' @return Logical. TRUE if the function is called from the top-level (interactive or script), FALSE otherwise.
#' @keywords internal
is_top_level <- function() {
  sys.nframe() <= 2
}

#' Resolve repository and owner for a measure
#'
#' @param measure Character. The measure name.
#' @param repo Character. Default repository name.
#' @param owner Character. Default owner name.
#'
#' @return List with elements 'repo' and 'owner'.
#' @keywords internal
resolve_measure_repo_owner <- function(measure, repo, owner) {
  repo  <- if (measure %in% c("income_groups", "country_list")) "Class" else repo
  owner <- if (measure == "nan") "PIP-Technical-Team" else owner
  list(repo = repo, owner = owner)
}

#' Recursively process dependencies for a measure
#'
#' For each dependency, calls \code{aux_fun()} recursively. Each dependency is only updated once per cascade.
#'
#' If \code{halt_on_dep_fail = TRUE}, any error in a dependency will halt the parent update and propagate the error. If \code{FALSE} (default), dependency errors are logged and the parent update continues.
#'
#' @param measure Character. The measure to process.
#' @param processed Environment. Tracks processed measures to avoid cycles.
#' @param owner Character. Repository owner.
#' @param tag Character. Release tag.
#' @param verbose Logical. Verbosity flag.
#' @param log Logical. Whether to log events.
#' @param halt_on_dep_fail Logical. Whether to halt parent update if a dependency fails.
#'
#' @return Invisibly returns NULL.
#' @keywords internal
process_dependencies <- function(measure,
                                 processed,
                                 owner,
                                 tag,
                                 verbose,
                                 log,
                                 log_name,
                                 halt_on_dep_fail = FALSE) {

  # Skip if already processed (prevents infinite loops)
  if (rlang::env_has(processed, measure)) return(invisible(NULL))

  # Mark as processed
  rlang::env_poke(processed, measure, TRUE)

  # Read dependency map
  dependencies_all <- read_dependencies(
    gh_user = "https://raw.githubusercontent.com",
    owner   = "PIP-Technical-Team"
  )

  deps <- dependencies_all[[measure]]
  if (is.null(deps)) deps <- character(0)

  # Recursively process dependencies
  for (dep in deps) {
    tryCatch(
      aux_fun(
        measure       = dep,
        repo          = NULL,
        processed     = processed,
        owner         = owner,
        tag           = tag,
        log_overwrite = TRUE,
        verbose       = verbose,
        log           = log,
        halt_on_dep_fail = halt_on_dep_fail
      ),
      error = function(e) {
        if (log) {
          pipfun::log_add(
            event   = "error",
            message = paste0("Failed to process dependency '", dep, "': ", e$message),
            name    = log_name,
            logmeta = list(step = "ERROR_DEP", measure = dep)
          )
        }
        if (halt_on_dep_fail) stop(e)
      }
    )
  }

  invisible(NULL)
}

#' Execute update for GitHub and Y drive
#'
#' @param measure Character. The measure to update.
#' @param update_gh Logical. Whether to update GitHub.
#' @param update_y Logical. Whether to update Y drive.
#' @param release_branch Character. Release branch name.
#' @param owner Character. Repository owner.
#' @param repo Character. Repository name.
#' @param tag Character. Release tag.
#' @param force Logical. Whether to force update.
#' @param verbose Logical. Verbosity flag.
#' @param log Logical. Whether to log events.
#'
#' @return Invisibly returns NULL.
#' @keywords internal
execute_update <- function(measure, update_gh, update_y, release_branch, owner, repo, tag, verbose, log, log_name) {

  # --- GitHub update ---
  if (update_gh) {
    pipfun::sync_release_branch(
      owner         = owner,
      repo          = repo,
      ref_branch    = "DEV",
      target_branch = release_branch,
      verbose       = verbose
    )

    if (log) {
      pipfun::log_add(
        event   = "update",
        message = paste0("Updated GitHub for: ", measure),
        name    = log_name,
        logmeta = list(step = "UPDATE_GH", measure = measure)
      )
    }
  }

  # --- Y drive update ---
  if (update_y) {
    func_name <- paste0("aux_", measure)

    if (!exists(func_name, envir = asNamespace("pipaux"))) {
      cli::cli_abort(paste0("Function '", func_name, "' does not exist in pipaux namespace"))
    }

    func <- get(func_name, envir = asNamespace("pipaux"))

    update_args <- list(
      action = "update",
      branch = release_branch,
      owner  = owner,
      tag    = tag,
      repo   = repo
    )

    filtered_args <- update_args[names(update_args) %in% names(formals(func))]

    tryCatch(
      {
        res <- do.call(func, filtered_args)

        # # If the aux save returned a fallback (NULL) result, treat as error.
        # if (is.null(res)) {
        #   if (log) {
        #     pipfun::log_add(
        #       event   = "error",
        #       message = paste0("Y drive update returned NULL (fallback) for: ", measure),
        #       name    = .piplogenv$active_aux_log,
        #       logmeta = list(step = "ERROR_Y_SAVE", measure = measure)
        #     )
        #   }
        #   stop(sprintf("Y drive update failed for measure '%s': returned NULL", measure))
        # }

        if (log) {
          log_name <- .piplogenv$active_aux_log
          
          pipfun::log_add(
            event   = "update",
            message = paste0("Updated Y drive for: ", measure),
            name    = log_name,
            logmeta = list(step = "UPDATE_Y", measure = measure)
          )
        }
      },
      error = function(e) {
        if (log) {
          pipfun::log_add(
            event   = "error",
            message = paste0("Error updating Y drive: ", e$message),
            name    = log_name,
            logmeta = list(step = "ERROR_Y", measure = measure)
          )
        }
        stop(e)
      }
    )
  }
}


#' Update auxiliary data for a measure and its dependencies
#'
#' This is the main exported function for updating auxiliary data. It handles
#' dependency resolution, update checks, and update execution for both GitHub and Y drive.
#'
#' Dependencies are always resolved recursively and each dependency is only updated once per cascade.
#'
#' Logging is performed to the log named \code{"pipaux_update_log"} by default. Use \code{pipfun::log_get("pipaux_update_log")} to retrieve logs.
#'
#' @param measure Character. The measure to update.
#' @param repo Character. Repository name (optional).
#' @param owner Character. Repository owner (optional).
#' @param processed Environment. Tracks processed measures (optional).
#' @param tag Character. Release tag (optional).
#' @param log Logical. Whether to log events.
#' @param log_overwrite Logical. Whether to overwrite existing log.
#' @param verbose Logical. Verbosity flag.
#' @param halt_on_dep_fail Logical. If \code{TRUE}, any error in a dependency will halt the parent update and propagate the error. If \code{FALSE} (default), dependency errors are logged and the parent update continues.
#'
#' @return Invisibly returns NULL.
#' @export
aux_fun <- function(measure,
                    repo      = NULL,
                    owner     = getOption("pipfun.ghowner"),
                    processed = new.env(parent = emptyenv()),
                    tag       = NULL,
                    log       = TRUE,
                    log_overwrite = TRUE,
                    log_name = NULL,
                    verbose   = FALSE,
                    halt_on_dep_fail = FALSE) {

  # Working release
  wrk_release <- get_from_auxenv(key = "wrk_release")
  release <- wrk_release$release
  identity <- wrk_release$identity
  release_branch <- paste0(release, "_", identity)

  if (is.null(tag)) tag <- release_branch
  if (is.null(repo)) repo <- paste0("aux_", measure)

   if (is_top_level() && log && is.null(log_name)) {
    log_name <- init_aux_log(overwrite = log_overwrite)
    on.exit(finalize_aux_log(), add = TRUE)
  } else if (log && is.null(log_name) && rlang::env_has(.piplogenv, "active_aux_log")) {
    log_name <- .piplogenv$active_aux_log
  }


  # Resolve special repo/owner
  ro <- resolve_measure_repo_owner(measure, repo, owner)
  repo <- ro$repo
  owner <- ro$owner

  # Process dependencies
  process_dependencies(measure, processed, owner, tag, verbose, log, log_name = log_name, halt_on_dep_fail)

  # Check update status
  check_result <- tryCatch(
    get_fs_status(measure = measure, repo = repo, owner = owner),
    error = function(e) {
      if (log) {
        pipfun::log_add(
          event   = "error",
          message = paste0("Check failed: ", e$message),
          name    = log_name,
          logmeta = list(step = "ERROR_CHECK", measure = measure)
        )
      }
      NULL
    }
  )

  if (is.null(check_result)) return(invisible(NULL))

  update_gh <- check_result$update_gh
  update_y  <- check_result$update_y

  # Early return if nothing to update
  if (!update_gh && !update_y) {
    if (log) {
      pipfun::log_add(
        event   = "info",
        message = paste0("No update needed for: ", measure),
        name    = log_name,
        logmeta = list(step = "END", measure = measure)
      )
    }
    cli::cli_alert_success("No updates needed")
    return(invisible(NULL))
  }

  # Execute updates
  execute_update(measure, update_gh, update_y, release_branch, owner, repo, tag, verbose, log, log_name = log_name)

  # Final log
  if (is_top_level() && log) {
    pipfun::log_add(
      event   = "success",
      message = "Measure and dependencies successfully updated",
      name    = log_name,
      logmeta = list(step = "END", measure = measure)
    )
    cli::cli_alert_success(
      paste0("Log available: ", cli::bg_br_cyan(cli::col_black("{.strong ", log_name, "}")),
             "\nUse {.code pipfun::log_get()} to access it. Or call {.code aux_log_last()} for a quick look at the most recent log.")
    )
  }

  invisible(NULL)
}