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
                         repo,
                         owner      = getOption("pipfun.ghowner"),
                         dev_branch = c("DEV", "DEV_v2"),
                         release_branch, #not sure this is needed
                         maindir) {

  # ---------------------------
  # Initialize output
  # ---------------------------

  update_gh <- FALSE
  update_y  <- FALSE

  # ---------------------------
  # Check GitHub status
  # ---------------------------

  dev_branch <- match.arg(dev_branch)

  # check if measure repo has release branch
  has_release_branch <- pipfun::get_repo_branches(owner = owner,
                            repo = paste0("aux_", measure))$has_release_branch

  release_up_to_date <- pipfun::compare_branch_content(owner = owner,
                                                       repo = paste0("aux_", measure),
                                                       branch1 = dev_branch,
                                                       branch2 = release_branch)$same_content

  update_gh <- !(has_release_branch && up_to_date)

  # ---------------------------
  # Check Y drive status
  # ---------------------------




}


# Get dependencies ####

#' #' Get dependencies of an auxiliary data measure
#' #'
#' #' This function retrieves the dependencies of a specified auxiliary data measure
#' #'
#' #' @param measure A character string specifying the auxiliary data measure
#' #' Must be one of the measures listed in the configuration file
#' #' @return A character vector of dependencies for the specified measure. If the
#' #' measure has no dependencies, an empty character vector is returned.
#' #' @keywords internal
#' #' @examples
#' #' # dependencies <- get_dependencies("cpi")
#' #' # print(dependencies)
#' #'
#' get_dependencies <- function(measure) {
#'   # Locate config file with dependencies
#'   yml_path <- system.file("extdata",
#'                           "config.yml",
#'                           package = "pipaux")
#'
#'   if (yml_path == "") {
#'     stop("YAML file not found")
#'   }
#'
#'   # Read the YAML file as a list
#'   yml_data <- yaml::read_yaml(yml_path)
#'
#'   # Check if the measure exists in the YAML data
#'   if (!measure %in% names(yml_data$default)) {
#'     cli::cli_abort("Measure '{measure}' not found in the YAML file.")
#'   }
#'
#'   # Extract dependencies for the given measure
#'   deps <- yml_data$default[[measure]]
#'
#'   # If no dependencies
#'   if (is.null(deps) || deps == "") {
#'     return(character(0)) # Return empty vector for no dependencies
#'   }
#'
#'   # Convert dependencies to a vector
#'   deps_vector <- unlist(strsplit(deps, ",\\s*"))
#'
#'   return(deps_vector)
#' }
#'
#' # get sha form gh
#'
#'
#' # Get file hash - TO CHECK IF NEEDED MAYBE NOT !!
#' # get_local_file_hash <- function(filepath) {
#' #   if (!file.exists(filepath)) {
#' #     stop("Local file not found.")
#' #   }
#' #   hash <- digest::digest(file = filepath, algo = "sha256", file = TRUE)
#' #   return(hash)
#' # }
#'
#' # add_file_sha <- function(file_path, format = "fst") {
#' #   if (!fs::file_exists(file_path)) {
#' #     stop("The file does not exist: ", file_path)
#' #   }
#' #
#' #   if (!format %in% c("fst", "dta", "qs")) {
#' #     cli::cli_abort("Invalid format. Supported formats are 'fst', 'dta', or 'qs'.")
#' #   }
#' #
#' #   # Compute the SHA based on format
#' #   file_sha <- switch(
#' #     format,
#' #     fst = digest::digest(fst::read_fst(file_path), algo = "sha1"),
#' #     dta = digest::digest(haven::read_dta(file_path), algo = "sha1"),
#' #     qs  = digest::digest(qs::qread(file_path), algo = "sha1")
#' #   )
#' #
#' #   # Assign the SHA as an attribute
#' #   attr(file_path, "sha") <- file_sha
#' #
#' #   return(file_path)
#' # }
#' #
#' #
