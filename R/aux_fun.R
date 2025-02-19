#' Call auxiliary function for a specified measure
#'
#' @param measure character: Name of the auxiliary data measure (e.g., "ppp")
#' @param ... additional arguments to pass to the auxiliary function
#'
#' @return The result of the auxiliary function call
aux_fun <- function(measure,
                    action = c("update", "load"),
                    repo = paste0("aux_", measure),
                    owner,
                    maindir = getOption("pipaux.working_dir"),
                    ...) {

  measure <- measure
  action <- match.arg(action)

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

  if (length(dependencies) >= 1) {
    lapply(dependencies, function(dep) {
      aux_fun(measure = dep,
              action  = action,
              owner   = owner, ...)
    })
  }

  update_status <- check_status(measure = measure,
                                repo    = repo,
                                owner   = owner,
                                maindir = maindir)

  update_gh <- update_status$update_gh
  update_y  <- update_status$update_y

  if (!update_gh & !update_y) {
    cli::cli_alert_info("No action required. GitHub and Y drive already up to date")
    return(NULL)
  }

  if (update_y == TRUE) {

    # Check GitHub first: is release branch updated with most recent version of DEV?
    if(update_gh == TRUE) {


    }

  }














  # Retrieve the function from the {pipaux} namespace
  # -- The namespace refers to the version currently loaded in the R session
  # func <- get(function_name,
  #             envir = asNamespace("pipaux"))
  #
  # # Call the function with additional arguments
  # func(...)
}

# Check status v0 ---- ####
# working well when gh is a single list of attributes ----- #

# Check status of an auxiliary data measure
check_status_v0 <- function(measure,
                         repo = paste0("aux_", measure),
                         owner      = getOption("pipfun.ghowner"),
                         #identity = c("PROD", "TEST", "INT"),
                         #release_branch = paste0(wrk_release$release, "_",
                         #                         wrk_release$identity), #not sure this is needed
                         maindir = getOption("pipaux.working_dir")) {

  # ---------------------------
  # 1. Check GitHub status
  # ---------------------------

  # Get repo branches and info
  gh_branches <- pipfun::get_repo_branches(owner = owner,
                                           repo  = repo)

  # check if measure repo has release branch
  has_release_branch <- gh_branches$has_release_branch

  if(has_release_branch) {
    release_branch = gh_branches$release_branch #fix to get the latest or choose which one

    release_up_to_date <- pipfun::compare_branch_content(owner = owner,
                                                         repo = paste0("aux_", measure),
                                                         branch1 = "DEV",
                                                         branch2 = release_branch)$same_content
  }


  # Update GitHub is TRUE if release branch has to be created or updated
  update_gh <- !(has_release_branch &&
                   release_up_to_date)

  if(update_gh) {
    return(list(
      update_gh = update_gh,
      update_y = TRUE
    ))
  } else {

    # ------------------------------------------------------
    # 2. Check Y drive status
    #    --conditional on gh update being FALSE
    # ------------------------------------------------------

    # DEBUG
    print(paste0("release_branch: ", release_branch))


    ## Construct file path

    #### TODO: Check file path structure ######
    y_file_path <- fs::path(maindir,
                            "aux_data",
                            release_branch,
                            measure, #folder
                            measure, #file
                            ext ="qs")

    # DEBUG
    print(paste0("y_file_path: ", y_file_path))


    ## Check if file exists

    # If file does not exist

    if (!fs::file_exists(y_file_path)) {
      cli::cli_alert_danger("File {y_file_path} does not exist.")
      return(list(
        update_gh = update_gh,
        update_y  = TRUE))
    } else {     #if file exists in Y drive

      # Get gh info from file saved in Y drive ----#

      ## Get list of attributes
      gh <- qs::qattributes(y_file_path)$gh

      # DEBUG
      #print(paste0("gh_raw_sha: ", gh$gh_raw_sha))

      # Check if raw sha has changed | GitHub ---- #

      ## Current sha in GH
      gh_sha <- tryCatch(
        {
          pipfun::get_file_info_from_gh(
            owner    = gh$owner,
            repo     = gh$repo,
            branch   = gh$branch,
            file_path = gh$file_path
          )$sha
        },
        error = function(e) {
          message("File not found or another error occurred: ", e$message)
          NULL  # or use 0?
        }
      )


      # DEBUG
      print(paste0("gh_sha: ", gh_sha))

      ## Raw sha in GH -as it was assigned to .qs file at time of saving
      y_sha <- gh_raw_sha <- gh$gh_raw_sha #this is the gh raw sha that has been saved as attribute to the file

      # Function sha ---- #

      ## Current sha
      fun_sha <- digest::digest(body(
        paste0("aux_", measure))
      )

      ## Raw sha -as it was assigned to .qs file at time of saving
      raw_fun_sha <- qs::qattributes(y_file_path)$raw_sha_fun

      # DEBUG
      print(paste0("fun_sha: ", fun_sha))
      print(paste0("raw_fun_sha: ", raw_fun_sha))

      # ---------------------------
      # Output & Return
      # ---------------------------


      # Update Y drive if file sha or fun sha has changed with respect to raw sha(s)
      update_y <- !(gh_sha     == y_sha &&
                      fun_sha == raw_fun_sha)

      return(list(update_gh = update_gh,
                  update_y  = update_y))

    } # end of checks between y drive and gh

  } # end of case when update gh is FALSE

} # end of check_status

# Check status v2 ---- ####
# attempt when gh is a list of lists ----- #
check_status_v1 <- function(measure,
                            repo       = paste0("aux_", measure),
                            owner      = getOption("pipfun.ghowner"),
                            maindir    = getOption("pipaux.working_dir")) {

  # ---------------------------
  # 1. Check GitHub status
  # ---------------------------

  # ---- NOTE ---- #
  # -------------- #

  # If the repo does not exist (as of now), it means there is no raw file in GitHub.
  # For example, the data file in the Y drive originates from formatting dependencies,
  # which are checked separately. Therefore, update_gh is set to FALSE.


  # Default to update_gh = FALSE in case of an error
  update_gh <- FALSE

  # Try to get repo branches and info
  gh_branches <- tryCatch(
    pipfun::get_repo_branches(owner = owner, repo = repo),
    error = function(e) {
      message("GitHub repository not found or an error occurred: ", e$message)
      return(NULL)
    }
  )

  # Proceed only if repo exists
  if (!is.null(gh_branches) && gh_branches$has_release_branch) {
    release_branch <- gh_branches$release_branch

    release_up_to_date <- pipfun::compare_branch_content(
      owner    = owner,
      repo     = paste0("aux_", measure),
      branch1  = "DEV",
      branch2  = release_branch
    )$same_content

    # Update GitHub is TRUE if release branch needs to be created or updated
    update_gh <- !release_up_to_date
  }


  if (update_gh) {
    return(list(
      update_gh = update_gh,
      update_y  = TRUE
    ))
  } else {

    # ------------------------------------------------------
    # 2. Check Y drive status
    #    --conditional on gh update being FALSE
    # ------------------------------------------------------

    # DEBUG
    print(paste0("release_branch: ", release_branch))

    ## Construct file path
    y_file_path <- fs::path(maindir,
                            "aux_data",
                            release_branch,
                            measure, # folder
                            measure, # file
                            ext = "qs")

    # DEBUG
    print(paste0("y_file_path: ", y_file_path))

    ## Check if file exists
    if (!fs::file_exists(y_file_path)) {
      cli::cli_alert_danger("File {y_file_path} does not exist.")
      return(list(
        update_gh = update_gh,
        update_y  = TRUE
      ))
    } else {  # If file exists in Y drive

      # Get gh info from file saved in Y drive ----#
      gh <- qs::qattributes(y_file_path)$gh

      # Ensure gh is always a named list of lists
      if (length(gh) > 0 && !is.list(gh[[1]])) {
        gh <- list(gh_list = gh) # Wrap list of elements into a list of lists
      }

      # Function to get SHA from GitHub
      get_gh_sha <- function(gh_entry) {
        tryCatch(
          {
            pipfun::get_file_info_from_gh(
              owner    = gh_entry$owner,
              repo     = gh_entry$repo,
              branch   = gh_entry$branch,
              file_path = gh_entry$file_path
            )$sha
          },
          error = function(e) {
            message("File not found or another error occurred: ", e$message)
            NULL  # or use 0?
          }
        )
      }

      # Apply function over all elements in gh
      gh_sha_list <- lapply(gh, function(entry) {
        list(
          gh_sha = get_gh_sha(entry),
          y_sha  = entry$gh_raw_sha  # Raw SHA stored in the .qs file
        )
      })

      # DEBUG
      print(gh_sha_list)

      # Function sha ---- #

      ## Current sha
      fun_sha <- digest::digest(body(
        paste0("aux_", measure))
      )

      ## Raw sha - as it was assigned to .qs file at time of saving
      raw_fun_sha <- qs::qattributes(y_file_path)$raw_sha_fun

      # DEBUG
      print(paste0("fun_sha: ", fun_sha))
      print(paste0("raw_fun_sha: ", raw_fun_sha))

      # ---------------------------
      # Output & Return
      # ---------------------------

      # Update Y drive if any gh_sha does not match y_sha OR if function SHA changed
      update_y <- any(sapply(gh_sha_list, function(x) x$gh_sha != x$y_sha)) ||
        !(fun_sha == raw_fun_sha)

      return(list(update_gh = update_gh,
                  update_y  = update_y))
    }
  }
}

#version 3

check_status <- function(measure,
                         repo       = paste0("aux_", measure),
                         owner      = getOption("pipfun.ghowner"),
                         maindir    = getOption("pipaux.working_dir"),
                         verbose    = FALSE) {

  update_gh <- FALSE

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





