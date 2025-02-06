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
                         release_branch = paste0(wrk_release$release, "_",
                                                 wrk_release$identity), #not sure this is needed
                         maindir = getOption("pipaux.working_dir")) {

  # ---------------------------
  # Check GitHub status
  # ---------------------------

  # check if measure repo has release branch
  has_release_branch <- pipfun::get_repo_branches(owner = owner,
                            repo = repo)$has_release_branch

  release_up_to_date <- pipfun::compare_branch_content(owner = owner,
                                                       repo = paste0("aux_", measure),
                                                       branch1 = "DEV",
                                                       branch2 = release_branch)$same_content

  # maybe get the release branch from here instead?

  # ---------------------------
  # Check Y drive status
  # ---------------------------

  # Get gh info from file saved in Y drive ----#

  ## Construct file path
  y_file_path <- fs::path(maindir,
                        "aux_data",
                        release_branch,
                        measure, #folder
                        measure, #file
                        ext ="qs")

  ## Get list of attributes
  gh <- qs::qattributes(y_file_path)$gh

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
      NULL  # or use 0 if you prefer
    }
  )


  ## Raw sha in GH -as it was assigned to .qs file at time of saving
  y_sha <- gh_raw_sha <- gh$gh_raw_sha #this is the gh raw sha that has been saved as attribute to the file

  # Function sha ---- #

  ## Current sha
  fun_sha <- digest::digest(deparse(
    paste0("aux_", measure))
  )

  ## Raw sha -as it was assigned to .qs file at time of saving
  raw_fun_sha <- qs::qattributes(y_file_path)$raw_sha_fun

  # ---------------------------
  # Output & Return
  # ---------------------------

  # Update GitHub is TRUE if release branch has to be created or updated
  update_gh <- !(has_release_branch &&
                   release_up_to_date)

  # Update Y drive if file sha or fun sha has changed with respect to raw sha(s)
  update_y <- !(gh_sha     == y_sha &&
                   fun_sha == raw_fun_sha)

  return(list(update_gh = update_gh,
              update_y  = update_y))

}

