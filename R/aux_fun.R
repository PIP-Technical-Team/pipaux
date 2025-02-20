#' Call auxiliary function for a specified measure
#'
#' @param measure character: Name of the auxiliary data measure (e.g., "ppp")
#' @param ... additional arguments to pass to the auxiliary function
#'
#' @return The result of the auxiliary function call
aux_fun <- function(measure,
                    action = c("update", "load"),
                    repo = paste0("aux_", measure),
                    branch = paste0(release, "_", identity),
                    owner,
                    release,
                    identity,
                    maindir = getOption("pipaux.working_dir"),
                    ...) {

  measure <- measure
  action <- match.arg(action)
  release_branch <- paste0(release, "_", identity)

  function_name <- paste0("aux_",
                          measure)

  # This checks if the function exists in the {pipaux} package namespace
  # -- The namespace corresponds to the version currently loaded in the R session
  # -- (development version loaded via devtools::load_all() or the installed version)

  if (!exists(function_name,
              envir = asNamespace("pipaux"))) {
    cli::cli_abort(paste0("Function '", function_name, "' does not exist in the '", "pipaux package."))
  }

  #   ____________________________________________________________
  #   Read Dependencies                                       ####

  dependencies <- read_dependencies(gh_user = "https://raw.githubusercontent.com",
                                    owner   = "PIP-Technical-Team")[[measure]]


  # Extract the relevant measure's dependencies
  # if (!measure %in% names(dependencies)) {
  #   cli::cli_abort(paste0("Measure '", measure, "' not found in dependencies."))
  # }
  #

  #   ____________________________________________________________
  #   Recursively call aux_fun for each dependency            ####

  if (length(dependencies) >= 1) {

    lapply(dependencies, function(dep) {
      aux_fun(measure  = measure,
              action   = action,
              repo     = repo,
              branch   = release_branch,
              owner    = owner,
              release  = release,
              identity = identity,
              maindir  = maindir,
              ...)
    })
  }

  #   ____________________________________________________________
  #   Check update status of measure, both in GH and Y:       ####

  check_status <- check_status(measure = measure,
                               repo    = repo,
                               owner   = owner,
                               maindir = maindir)

  update_gh <- check_status$update_gh
  update_y  <- check_status$update_y

  if (!update_gh & !update_y) {
    cli::cli_alert_info("No action required. GitHub and Y drive already up to date")
    return(NULL)
  }

  if (update_y == TRUE) {

    #   ____________________________________________________________
    #   First check and update GitHub                           ####

    if(update_gh == TRUE) {

      pipfun::sync_release_branch(owner      = owner,
                                  repo       = repo,
                                  ref_branch = "DEV",
                                  release    = release,
                                  identity   = identity)

    }

    #   ____________________________________________________________
    #   Update Y drive                                          ####

    # Retrieve the function from the {pipaux} namespace
    # -- The namespace refers to the version currently loaded in the R session

    func <- get(function_name,
                envir = asNamespace("pipaux"))

    # Call the function with additional arguments
    func(action  = action,
         maindir = maindir,
         owner   = owner,
         branch  = release_branch)

  }
}


#version 3

check_status <- function(measure,
                         repo       = paste0("aux_", measure),
                         owner      = getOption("pipfun.ghowner"),
                         maindir    = getOption("pipaux.working_dir"),
                         verbose    = FALSE) {

  update_gh <- TRUE

  gh_branches <- tryCatch(
    pipfun::get_repo_branches(owner = owner, repo = repo),
    error = function(e) {
      if (verbose) cli::cli_alert_danger("GitHub repository not found or an error occurred: {e$message}")
      return(NULL)
    }
  )

  if (!is.null(gh_branches) && gh_branches$has_release_branch) {
    release_branch <- gh_branches$release_branch

    release_up_to_date <- pipfun::compare_branch_content(
      owner    = owner,
      repo     = paste0("aux_", measure),
      branch1  = "DEV",
      branch2  = release_branch
    )$same_content

    update_gh <- !release_up_to_date
  }

  if (update_gh) {
    return(list(update_gh = update_gh, update_y = TRUE))
  } else {
    y_file_path <- fs::path(maindir, "aux_data", release_branch, measure, measure, ext = "qs")

    if (verbose) cli::cli_alert_info("Checking file: {y_file_path}")

    if (!fs::file_exists(y_file_path)) {
      cli::cli_alert_danger("File {y_file_path} does not exist.")
      return(list(update_gh = update_gh, update_y = TRUE))
    } else {
      gh <- qs::qattributes(y_file_path)$gh

      if (length(gh) > 0 && !is.list(gh[[1]])) {
        gh <- list(gh_list = gh)
      }

      get_gh_sha <- function(gh_entry) {
        tryCatch(
          pipfun::get_file_info_from_gh(
            owner    = gh_entry$owner,
            repo     = gh_entry$repo,
            branch   = gh_entry$branch,
            file_path = gh_entry$file_path
          )$sha,
          error = function(e) {
            if (verbose) cli::cli_alert_danger("File not found or another error occurred: {e$message}")
            NULL
          }
        )
      }

      gh_sha_list <- lapply(gh, function(entry) {
        list(
          gh_sha = get_gh_sha(entry),
          y_sha  = entry$gh_raw_sha
        )
      })

      if (verbose) cli::cli_alert_info("GitHub and Y drive SHAs: {gh_sha_list}")

      fun_sha <- digest::digest(body(paste0("aux_", measure)))
      raw_fun_sha <- qs::qattributes(y_file_path)$raw_sha_fun

      if (verbose) {
        cli::cli_alert_info("Computed function SHA: {fun_sha}")
        cli::cli_alert_info("Stored function SHA: {raw_fun_sha}")
      }

      update_y <- any(sapply(gh_sha_list, function(x) x$gh_sha != x$y_sha)) ||
        !(fun_sha == raw_fun_sha)

      return(list(update_gh = update_gh, update_y = update_y))
    }
  }
}





