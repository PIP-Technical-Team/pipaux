#' Detect if the current call is top-level
#'
#' @description
#' Returns `TRUE` if the function is being called from the top-level environment
#' (interactive session or script), `FALSE` if called from within another function.
#'
#' @return Logical. `TRUE` if called from the top level, `FALSE` otherwise.
#' @keywords internal
is_top_level <- function() {
  sys.nframe() <= 2
}

#' Resolve repository and owner for a measure
#'
#' @description
#' Applies special-case overrides for measures that live in non-standard
#' repositories or under a different GitHub owner.
#' `"income_groups"` and `"country_list"` resolve to repo `"Class"`;
#' `"nan"` always resolves to owner `"PIP-Technical-Team"`.
#'
#' @param measure Character. The measure name.
#' @param repo Character. Default repository name.
#' @param owner Character. Default owner name.
#'
#' @return A named list with elements `repo` and `owner`.
#' @keywords internal
resolve_measure_repo_owner <- function(measure, repo, owner) {
  repo  <- if (measure %in% c("income_groups", "country_list")) "Class" else repo
  owner <- if (measure == "nan") "PIP-Technical-Team" else owner
  list(repo = repo, owner = owner)
}

#' Recursively process dependencies for a measure
#'
#' @description
#' Reads the dependency graph and calls [aux_fun()] for each upstream dependency
#' of `measure` that has not yet been processed. Tracks processed measures via
#' the `processed` environment to prevent cyclic re-processing.
#'
#' @param measure Character. The measure whose dependencies should be processed.
#' @param processed Environment. Tracks already-processed measures to avoid
#'   cycles. Modified in-place.
#' @param owner Character. GitHub repository owner.
#' @param tag Character. Release tag used when updating dependencies.
#' @param verbose Logical. If `TRUE`, prints progress messages.
#' @param log Logical. If `TRUE`, log events are recorded.
#' @param log_name Character or `NULL`. Explicit log name to use. If `NULL`,
#'   logging is skipped even when `log = TRUE`.
#' @param halt_on_dep_fail Logical. If `TRUE`, stops execution when a dependency
#'   update fails. If `FALSE`, errors are logged and processing continues.
#'
#' @return Invisibly returns `NULL`. The `processed` environment is modified
#'   as a side effect.
#' @keywords internal
process_dependencies <- function(measure,
                                 processed,
                                 owner,
                                 tag,
                                 verbose,
                                 log,
                                 log_name = NULL,
                                 halt_on_dep_fail = FALSE) {

  # Capture log conditions early
  use_log <- log && !is.null(log_name)

  if (rlang::env_has(processed, measure)) return(invisible(NULL))
  rlang::env_poke(processed, measure, TRUE)

  dependencies_all <- read_dependencies(
    gh_user = "https://raw.githubusercontent.com",
    owner   = "PIP-Technical-Team"
  )

  deps <- dependencies_all[[measure]]
  if (is.null(deps)) deps <- character(0)

  for (dep in deps) {
    tryCatch(
      aux_fun(
        measure       = dep,
        repo          = NULL,
        processed     = processed,
        owner         = owner,
        tag           = tag,
        log_overwrite = FALSE,
        verbose       = verbose,
        log           = log,
        halt_on_dep_fail = halt_on_dep_fail,
        log_name      = log_name
      ),
      error = function(e) {
        if (use_log) {
          pipfun::log_add(
            event   = "error",
            message = paste0("Failed to process dependency '", dep, "': ", e$message),
            name    = log_name,
            args = list(),
            logmeta = list(step = "ERROR_DEP", measure = dep)
          )
        }
        if (halt_on_dep_fail) stop(e)
      }
    )
  }

  invisible(NULL)
}

#' Execute GitHub and Y-drive updates for a measure
#'
#' @description
#' Performs the actual update steps for a measure:
#' - If `update_gh = TRUE`, syncs the release branch on GitHub from `DEV` via
#'   [pipfun::sync_release_branch()].
#' - If `update_y = TRUE`, calls the measure's generator function
#'   (`aux_<measure>()`) with the appropriate arguments to refresh the Y-drive
#'   data file.
#'
#' @param measure Character. The measure name.
#' @param update_gh Logical. If `TRUE`, the GitHub release branch is updated.
#' @param update_y Logical. If `TRUE`, the Y-drive data file is regenerated.
#' @param release_branch Character. Name of the release branch to sync to.
#' @param owner Character. GitHub repository owner.
#' @param repo Character. GitHub repository name.
#' @param tag Character. Release tag passed to the generator function.
#' @param verbose Logical. If `TRUE`, prints progress messages.
#' @param log Logical. If `TRUE`, log events are recorded.
#' @param log_name Character or `NULL`. Leave it NULL to make the name be internally generated.
#'
#' @return Invisibly returns `NULL`.
#' @keywords internal
execute_update <- function(measure, update_gh, update_y, release_branch, owner, repo, tag, verbose, log, log_name = NULL) {

  # Capture log_name early to avoid promise evaluation issues
  use_log <- log && !is.null(log_name)

  # --- GitHub update ---
  if (update_gh) {
    pipfun::sync_release_branch(
      owner         = owner,
      repo          = repo,
      ref_branch    = "DEV",
      target_branch = release_branch,
      verbose       = verbose
    )

    if (use_log) {
      pipfun::log_add(
        event   = "update",
        message = paste0("Updated GitHub for: ", measure),
        name    = log_name,
        args = list(),
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

        if (use_log) {
          pipfun::log_add(
            event   = "update",
            message = paste0("Updated Y drive for: ", measure),
            name    = log_name,
            args = list(),
            logmeta = list(step = "UPDATE_Y", measure = measure)
          )
        }
      },
      error = function(e) {
        if (use_log) {
          pipfun::log_add(
            event   = "error",
            message = paste0("Error updating Y drive: ", e$message),
            name    = log_name,
            args = list(),
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
#' @description
#' Main entry point for updating a single auxiliary data measure. Resolves
#' the release branch from the working environment, processes upstream
#' dependencies via [process_dependencies()], checks whether updates are
#' needed via [get_fs_status()], and calls [execute_update()] if required.
#' Top-level calls initialise and finalise the audit log automatically.
#'
#' @param measure Character. Name of the auxiliary data measure to update
#'   (e.g., `"cpi"`, `"ppp"`).
#' @param repo Character or `NULL`. GitHub repository name. Defaults to
#'   `"aux_<measure>"` if `NULL`.
#' @param owner Character. GitHub repository owner. Defaults to
#'   `getOption("pipfun.ghowner")`.
#' @param processed Environment. Tracks already-processed measures to avoid
#'   cyclic dependency updates. Defaults to a fresh empty environment.
#' @param tag Character or `NULL`. Release tag passed to the generator
#'   function. Defaults to the release branch name if `NULL`.
#' @param log Logical. If `TRUE`, audit log events are recorded.
#'   Default is `TRUE`.
#' @param log_overwrite Logical. If `TRUE`, overwrites an existing log with
#'   the same name. Default is `TRUE`.
#' @param verbose Logical. If `TRUE`, prints progress messages. Default is
#'   `FALSE`.
#' @param halt_on_dep_fail Logical. If `TRUE`, stops execution when a
#'   dependency update fails. Default is `FALSE`.
#' @param log_name Character or `NULL`. Leave it NULL. 
#'
#' @return Invisibly returns `NULL`.
#' @export
#'
#' @examples
#' \dontrun{
#' aux_fun(measure = "cpi")
#' aux_fun(measure = "ppp", verbose = TRUE)
#' }
aux_fun <- function(measure,
                    repo      = NULL,
                    owner     = getOption("pipfun.ghowner"),
                    processed = new.env(parent = emptyenv()),
                    tag       = NULL,
                    log       = TRUE,
                    log_overwrite = TRUE,
                    verbose   = FALSE,
                    halt_on_dep_fail = FALSE,
                    log_name  = NULL) {

  wrk_release <- get_from_auxenv(key = "wrk_release")
  release <- wrk_release$release
  identity <- wrk_release$identity
  release_branch <- paste0(release, "_", identity)

  if (is.null(tag)) tag <- release_branch
  if (is.null(repo)) repo <- paste0("aux_", measure)

  # Top-level logging only
  if (is_top_level() && log && is.null(log_name)) {
    log_name <- init_aux_log(overwrite = log_overwrite)
    on.exit(finalize_aux_log(), add = TRUE)
  }

  # Resolve special repo/owner
  ro <- resolve_measure_repo_owner(measure, repo, owner)
  repo <- ro$repo
  owner <- ro$owner

  # Process dependencies
  process_dependencies(measure, processed, owner, tag, verbose, log, log_name, halt_on_dep_fail)

  # Check update status
  check_result <- tryCatch(
    get_fs_status(measure = measure, repo = repo, owner = owner),
    error = function(e) {
      if (log && !is.null(log_name)) {
        pipfun::log_add(
          event   = "error",
          message = paste0("Check failed: ", e$message),
          name    = log_name,
          args = list(),
          logmeta = list(step = "ERROR_CHECK", measure = measure)
        )
      }
      NULL
    }
  )

  if (is.null(check_result)) return(invisible(NULL))

  update_gh <- check_result$update_gh
  update_y  <- check_result$update_y

  if (!update_gh && !update_y) {
    if (log && !is.null(log_name)) {
      pipfun::log_add(
        event   = "info",
        message = paste0("No update needed for: ", measure),
        name    = log_name,
        args = list(),
        logmeta = list(step = "END", measure = measure)
      )
    }
    cli::cli_alert_success("No updates needed")
    return(invisible(NULL))
  }

  # Execute updates
  execute_update(measure, update_gh, update_y, release_branch, owner, repo, tag, verbose, log, log_name)

  # Final top-level log
  if (is_top_level() && log && !is.null(log_name)) {
    pipfun::log_add(
      event   = "success",
      message = "Measure and dependencies successfully updated",
      name    = log_name,
      args = list(),
      logmeta = list(step = "END", measure = measure)
    )
    cli::cli_alert_success(
      paste0("Log available: ", cli::bg_br_cyan(cli::col_black("{.strong ", log_name, "}"))),
      "\nUse {.code pipfun::log_get()} to access it. Or call {.code aux_log_last()} for a quick look at the most recent log."
    )
  }

  invisible(NULL)
}



#' Update specified auxiliary data measures
#'
#' @description
#' Updates one or more auxiliary data measures and their dependencies, in
#' dependency order. If `measures` is `NULL`, all measures available under
#' `owner` are updated. Resolves dependency order by reading the shared
#' dependency manifest via [read_dependencies()].
#'
#' @param measures Character vector or `NULL`. Names of measures to update
#'   (e.g., `c("cpi", "ppp")`). If `NULL`, all available measures are updated
#'   in dependency order.
#' @param repo Character or `NULL`. Default repository name. If `NULL`, a
#'   repository name is derived as `"aux_<measure>"` for each measure.
#' @param owner Character. GitHub repository owner. Defaults to
#'   `getOption("pipfun.ghowner")`.
#' @param tag Character or `NULL`. Release tag passed to the generator
#'   function. Defaults to the release branch name if `NULL`.
#' @param log Logical. If `TRUE`, audit log events are recorded.
#'   Default is `TRUE`.
#' @param log_overwrite Logical. If `TRUE`, overwrites an existing log with
#'   the same name. Default is `TRUE`.
#' @param verbose Logical. If `TRUE`, prints progress messages. Default is
#'   `FALSE`.
#' @param halt_on_dep_fail Logical. If `TRUE`, stops execution when a
#'   dependency update fails. Default is `FALSE`.
#' @param log_save Logical. If `TRUE`, persists the final log to the
#'   auxiliary metadata path via [pipfun::log_save()]. Default is `FALSE`.
#' @param log_name Character or `NULL`. Leave it NULL.
#'
#' @return Invisibly returns `NULL`.
#' @export
#'
#' @examples
#' \dontrun{
#' update_aux_measures()
#' update_aux_measures(measures = c("cpi", "ppp"), verbose = TRUE)
#' update_aux_measures(measures = "gdp", log_save = TRUE)
#' }
update_aux_measures <- function(
  measures = NULL,
  repo = NULL,
  owner = getOption("pipfun.ghowner"),
  tag = NULL,
  log = TRUE,
  log_overwrite = TRUE,
  verbose = FALSE,
  halt_on_dep_fail = FALSE,
  log_save = FALSE,
  log_name = NULL
) {

  # -------------------------------------------------------------------
  # 1. Resolve available measures from GitHub
  # -------------------------------------------------------------------

  if (is.null(owner)) {
    stop("Owner must be provided or set via option 'pipfun.ghowner'.")
  }

  # All aux_* repos under owner
  all_measures_raw <- gh::gh(
    "GET /users/{username}/repos",
    username = owner
  ) |>
    vapply("[[", "", "name") |>
    grep("^aux_", x = _, value = TRUE) |>
    sub("^aux_", "", x = _)

  # -------------------------------------------------------------------
  # 2. Resolve dependency order
  # -------------------------------------------------------------------

  dependency_order <- names(
    read_dependencies(
      gh_user = "https://raw.githubusercontent.com",
      owner   = "PIP-Technical-Team"
    )
  )

  # Keep only repos that actually exist
  available_measures <- intersect(dependency_order, all_measures_raw)

  if (length(available_measures) == 0) {
    stop("No auxiliary measures found.")
  }

  # -------------------------------------------------------------------
  # 3. Determine final measure list
  # -------------------------------------------------------------------

  if (is.null(measures)) {
    final_measures <- available_measures
  } else {

    unknown <- setdiff(measures, available_measures)
    if (length(unknown) > 0) {
      stop(
        "Unknown measure(s): ",
        paste(unknown, collapse = ", ")
      )
    }

    # Preserve dependency order
    final_measures <- available_measures[
      available_measures %in% measures
    ]
  }

  if (length(final_measures) == 0) {
    message("No measures to update.")
    return(invisible(NULL))
  }

  if (verbose) {
    message("Measures to update (in dependency order):")
    message(paste(final_measures, collapse = ", "))
  }

  # -------------------------------------------------------------------
  # 4. Initialize logging (top-level only)
  # -------------------------------------------------------------------

  is_top <- is_top_level()

  if (is_top && log) {

    if (is.null(log_name)) {
      log_name <- init_aux_log(overwrite = log_overwrite)
    }

    on.exit({
      finalize_aux_log()
    }, add = TRUE)
  }

  # -------------------------------------------------------------------
  # 5. Execute updates
  # -------------------------------------------------------------------

  processed <- new.env(parent = emptyenv())

  for (measure in final_measures) {

    if (verbose) message("Updating: ", measure)

    tryCatch({

      aux_fun(
        measure = measure,
        repo = repo,
        owner = owner,
        processed = processed,
        tag = tag,
        log = log,
        log_overwrite = log_overwrite,
        verbose = verbose,
        halt_on_dep_fail = halt_on_dep_fail,
        log_name = log_name
      )

    }, error = function(e) {

      if (log && !is.null(log_name)) {
        pipfun::log_add(
          event = "error",
          message = paste0(
            "Failed to update measure '",
            measure,
            "': ",
            e$message
          ),
          name = log_name,
          args = list(),
          logmeta = list(
            step = "ERROR_UPDATE",
            measure = measure
          )
        )
      }

      if (halt_on_dep_fail) {
        stop(e)
      }
    })
  }

  # -------------------------------------------------------------------
  # 6. Retrieve log object into .piplogenv
  # -------------------------------------------------------------------

  if (log && !is.null(log_name)) {
    log_obj <- pipfun::log_get(log_name)
    rlang::env_poke(.piplogenv, log_name, log_obj)
  }

  # -------------------------------------------------------------------
  # 7. Persist log if requested
  # -------------------------------------------------------------------

  if (log_save && !is.null(log_name)) {
    pipfun::log_save(
      name = log_name,
      alias = get_from_auxenv("aux_meta_alias")
    )
  }

  invisible(NULL)
}