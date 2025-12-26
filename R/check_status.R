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


check_y_drive_status <- function(measure,
                                 verbose = TRUE) {

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

  attr_list <- attributes(dt)
  gh        <- attr_list$gh

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
  fun_name <- paste0("aux_", measure)

  if (!exists(fun_name, mode = "function")) {
    if (verbose) {
      cli::cli_alert_danger(
        "Aux function {.strong {fun_name}} not found."
      )
    }
    return(list(update_y = TRUE, reason = "Aux function missing"))
  }

  fun          <- get(fun_name, mode = "function")
  fun_sha      <- digest::digest(body(fun))
  stored_sha   <- attr_list$raw_sha_fun

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


check_status <- function(measure,
                         repo    = paste0("aux_", measure),
                         owner   = getOption("pipfun.ghowner"),
                         verbose = TRUE) {

  wrk_release <- get_from_auxenv("wrk_release")

  release_branch <- paste0(
    wrk_release$release,
    "_",
    wrk_release$identity
  )

  if (verbose) {
    cli::cli_h1("Checking Status for {measure}")
  }

  # 1️⃣ GitHub status
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

    return(invisible(list(
      update_gh = TRUE,
      update_y  = TRUE
    )))
  }

  # 2️⃣ Y-drive status (only if GH is FALSE or NA)
  y_status <- check_y_drive_status(
    measure = measure,
    verbose = verbose
  )

  if (verbose) {
    cli::cli_h1("Summary")
    cli::cli_alert_info("Update GitHub: {.strong {gh_status$update_gh}}")
    cli::cli_alert_info("Update Y drive: {.strong {y_status$update_y}}")
  }

  invisible(list(
    update_gh = gh_status$update_gh,
    update_y  = y_status$update_y
  ))
}

get_fs_status <- function(measure,
                          repo    = paste0("aux_", measure),
                          owner   = getOption("pipfun.ghowner")) {

  status <- check_status(
    measure = measure,
    repo    = repo,
    owner   = owner,
    verbose = FALSE
  )

  status
}