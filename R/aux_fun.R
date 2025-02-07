#' Call auxiliary function for a specified measure
#'
#' @param measure character: Name of the auxiliary data measure (e.g., "ppp")
#' @param ... additional arguments to pass to the auxiliary function
#'
#' @return The result of the auxiliary function call.
aux_fun <- function(measure,
                    ...) {

  function_name <- paste0("aux_",
                          measure)

  # This checks if the function exists in the {pipaux} package namespace
  # -- The namespace corresponds to the version currently loaded in the R session
  # -- (development version loaded via devtools::load_all() or the installed version)

  if (!exists(function_name,
              envir = asNamespace("pipaux"))) {
    cli::cli_abort(paste0("Function '", function_name, "' does not exist in the '", "pipaux package."))
  }

  # Retrieve the function from the {pipaux} namespace
  # -- The namespace refers to the version currently loaded in the R session
  func <- get(function_name,
              envir = asNamespace("pipaux"))

  # Call the function with additional arguments
  func(...)
}

# Check status v0  ####

# Check status of an auxiliary data measure
check_status <- function(measure,
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
      print(paste0("gh_raw_sha: ", gh$gh_raw_sha))

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

