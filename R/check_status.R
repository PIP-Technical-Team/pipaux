#' @title Check GitHub status for an auxiliary data measure
#'
#' @description
#' Checks if the release branch for a measure is present and up to date with DEV on GitHub.
#'
#' @param measure Character. Name of the auxiliary data measure (e.g., "cpi").
#' @param repo Character. GitHub repository name.
#' @param owner Character. GitHub repository owner.
#' @param release_branch Character. Name of the release branch to check.
#' @param verbose Logical. If TRUE, prints status messages.
#'
#' @return A list with `update_gh` (logical or NA) and `reason` (character).
#' @keywords internal
#'
#' @examples
#' check_github_status("cpi", "aux_cpi", "myuser", "20250101_TEST")
check_github_status <- function(measure,
                                repo,
                                owner,
                                release_branch,
                                verbose = TRUE) {

  # Default: unknown
  update_gh <- NA
  reason    <- "Unknown GitHub status"

  gh_branches <- tryCatch(
    pipfun::get_repo_branches(owner = owner, repo = repo),
    error = function(e) {
      if (verbose) {
        cli::cli_alert_danger(
          "GitHub repository not found or unreachable: {e$message}"
        )
      }
      return(NULL)
    }
  )

  if (is.null(gh_branches)) {
    return(list(update_gh = NA, reason = "GitHub unreachable"))
  }

  if (!(release_branch %in% gh_branches$release_branches)) {
    if (verbose) {
      cli::cli_alert_warning(
        "GitHub branch {.strong {release_branch}} does not exist. Update required."
      )
    }
    return(list(update_gh = TRUE, reason = "Release branch missing"))
  }

  cmp <- tryCatch(
    pipfun::compare_branch_content(
      owner    = owner,
      repo     = repo,
      branch1  = "DEV",
      branch2  = release_branch,
      verbose  = FALSE
    ),
    error = function(e) {
      if (verbose) {
        cli::cli_alert_danger(
          "Failed to compare GitHub branches: {e$message}"
        )
      }
      return(NULL)
    }
  )

  if (is.null(cmp)) {
    return(list(update_gh = NA, reason = "Branch comparison failed"))
  }

  if (isTRUE(cmp$same_content)) {
    if (verbose) {
      cli::cli_alert_success(
        "GitHub branch {.strong {release_branch}} is up to date with DEV."
      )
    }
    update_gh <- FALSE
    reason    <- "GitHub up to date"
  } else {
    if (verbose) {
      cli::cli_alert_warning(
        "GitHub branch {.strong {release_branch}} is outdated. Update required."
      )
    }
    update_gh <- TRUE
    reason    <- "GitHub branch outdated"
  }

  list(update_gh = update_gh, reason = reason)
}

#' @title Check Y-drive status for an auxiliary data measure
#'
#' @description
#' Checks if the local Y-drive version of the auxiliary data is in sync with GitHub and the function code.
#'
#' @param measure Character. Name of the auxiliary data measure (e.g., "cpi").
#' @param verbose Logical. If TRUE, prints status messages.
#'
#' @return A list with `update_y` (logical) and `reason` (character).
#' @keywords internal
#'
#' @examples
#' check_y_drive_status("cpi")
check_y_drive_status <- function(measure,
                                 verbose = TRUE) {
  
  # Construct the path to the aux data file
  ext <- "qs2"
  sidecar_path <- fs::path(get_from_auxenv("aux_data_path"), measure, ext = ext)

  # Read sidecar metadata
  sidecar <- tryCatch(
    stamp::st_read_sidecar(sidecar_path),
    error = function(e) {
      if (verbose) {
        cli::cli_alert_danger(
          "Sidecar for measure '{measure}' not found: {e$message}"
        )
      }
      return(NULL)
    }
  )

  if (is.null(sidecar)) {
    return(list(update_y = TRUE, reason = "Aux sidecar missing"))
  }

  # Try loading aux data ONCE
  dt <- tryCatch(
    pipload::load_aux_data(measure = measure),
    error = function(e) {
      if (verbose) {
        cli::cli_alert_danger(
          "Aux data for measure '{measure}' not found: {e$message}"
        )
      }
      return(NULL)
    }
  )

  if (is.null(dt)) {
    return(list(update_y = TRUE, reason = "Aux data missing"))
  }

  gh        <- sidecar$gh

  if (is.null(gh) || length(gh) == 0) {
    if (verbose) {
      cli::cli_alert_danger("Missing or empty 'gh' attribute in aux data.")
    }
    return(list(update_y = TRUE, reason = "Missing GitHub metadata"))
  }

  # Normalize: expect a list of entries
  if (!is.list(gh[[1]])) {
    gh <- list(gh)
  }

  # Helper to fetch GitHub SHA
  get_gh_sha <- function(entry) {
    tryCatch(
      pipfun::get_file_info_from_gh(
        owner     = entry$owner,
        repo      = entry$repo,
        branch    = entry$branch,
        file_path = entry$file_path
      )$sha,
      error = function(e) {
        if (verbose) {
          cli::cli_alert_danger(
            "Failed to retrieve GitHub SHA: {e$message}"
          )
        }
        return(NA_character_)
      }
    )
  }

  sha_mismatch <- FALSE

  for (entry in gh) {

    gh_sha <- get_gh_sha(entry)
    y_sha  <- entry$gh_raw_sha

    if (is.na(gh_sha) || is.na(y_sha) || gh_sha != y_sha) {
      sha_mismatch <- TRUE
      break
    }
  }

  # Function SHA comparison
  # fun_name <- paste0("aux_", measure)
  fun_name <- sidecar$code_label

  if (is.null(fun_name) || !exists(fun_name, mode = "function")) {
  return(list(update_y = TRUE, reason = "Generator function missing"))
}
  
  fun <- get(fun_name, mode = "function")
  fun_sha <- hash_code(fun)
  stored_sha <- sidecar$code_hash
  
  fun_changed <- is.null(stored_sha) || fun_sha != stored_sha

  if (verbose) {
    if (!fun_changed) {
      cli::cli_alert_success(
        "Current and stored function SHAs match."
      )
    } else {
      cli::cli_alert_warning(
        "Function SHA mismatch detected."
      )
    }
  }

  update_y <- sha_mismatch || fun_changed

  list(
    update_y = update_y,
    reason   = if (update_y) "Y drive out of sync" else "Y drive up to date"
  )
}

#' @title Check overall status for an auxiliary data measure
#'
#' @description
#' Checks both GitHub and Y-drive status for a measure and summarizes whether updates are needed.
#'
#' @param measure Character. Name of the auxiliary data measure (e.g., "cpi").
#' @param repo Character. GitHub repository name. Defaults to "aux_<measure>".
#' @param owner Character. GitHub repository owner.
#' @param verbose Logical. If TRUE, prints status messages.
#' @param include_reason Logical. If TRUE, includes reasons in output.
#'
#' @return An (invisible) list with `update_gh`, `update_y` (logical or NA), and optionally `gh_reason`, `y_reason`.
#' @keywords internal
#'
#' @examples
#' check_status("cpi")
check_status <- function(measure,
                         repo    = paste0("aux_", measure),
                         owner   = getOption("pipfun.ghowner"),
                         verbose = TRUE,
                         include_reason = FALSE) {

  wrk_release <- get_from_auxenv("wrk_release")

  release_branch <- paste0(
    wrk_release$release,
    "_",
    wrk_release$identity
  )

  # Resolve special repo/owner for certain measures
  ro <- resolve_measure_repo_owner(measure, repo, owner)
  repo <- ro$repo
  owner <- ro$owner

  if (verbose) {
    cli::cli_h1("Checking Status for {measure}")
  }

  # 1️ GitHub status
  gh_status <- check_github_status(
    measure        = measure,
    repo           = repo,
    owner          = owner,
    release_branch = release_branch,
    verbose        = verbose
  )

  # POLICY: GitHub update ⇒ Y drive update
  if (isTRUE(gh_status$update_gh)) {

    if (verbose) {
      cli::cli_h1("Summary")
      cli::cli_alert_info("Update GitHub: TRUE")
      cli::cli_alert_info("Update Y drive: TRUE")
    }

    out <- list(
      update_gh = TRUE,
      update_y  = TRUE
    )
    if (include_reason) {
      out$gh_reason <- gh_status$reason
      out$y_reason  <- "Policy: GitHub update implies Y drive update"
    }
    return(invisible(out))
  }

  # 2️ Y-drive status (only if GH is FALSE or NA)
  y_status <- check_y_drive_status(
    measure = measure,
    verbose = verbose
  )

  if (verbose) {
    cli::cli_h1("Summary")
    cli::cli_alert_info("Update GitHub: {.strong {gh_status$update_gh}}")
    cli::cli_alert_info("Update Y drive: {.strong {y_status$update_y}}")
  }

  out <- list(
    update_gh = gh_status$update_gh,
    update_y  = y_status$update_y
  )
  if (include_reason) {
    out$gh_reason <- gh_status$reason
    out$y_reason  <- y_status$reason
  }

  invisible(out)
}

#' @title Get file system status for an auxiliary data measure
#'
#' @description
#' Wrapper for \code{check_status()} with \code{verbose = FALSE}.
#'
#' @param measure Character. Name of the auxiliary data measure (e.g., "cpi").
#' @param repo Character. GitHub repository name. Defaults to "aux_<measure>".
#' @param owner Character. GitHub repository owner.
#'
#' @return A list with `update_gh` and `update_y` (logical or NA).
#' @keywords internal
#'
#' @examples
#' get_fs_status("cpi")
get_fs_status <- function(measure,
                          repo    = paste0("aux_", measure),
                          owner   = getOption("pipfun.ghowner"),
                          include_reason = FALSE) {

  status <- check_status(
    measure = measure,
    repo    = repo,
    owner   = owner,
    verbose = FALSE,
    include_reason = include_reason
  )

  status
}