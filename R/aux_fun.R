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

# Check dependencies v0 test ####


check_deps <- function(measure,
                       ...) {

  # ------------------- #
  # Get deps ~~~~
  # ------------------- #

  get_deps <- get_dependencies(measure)

  # ------------------- #
  # Check deps ~~~~
  # ------------------- #

  # 3 scenarios:
  #   1. Raw data has changed in GitHub but not updated in Y drive
  #   2. Raw data **of one of its dependencies** has changed in GitHub but not updated in Y drive
  #   (3. TBC) the update_* function has changed
  #         or the update_* function of one/more deps has changed

  # TODO: return names of measures to update or TRUE if any dep has changed

  # Case 1

  # GitHub sha #####
  # Get sha of the release branch

  gh_raw_sha <- pipfun::get_branch_info_from_gh(
    branch = "TODO",     # TODO this should be the release branch, how do we identify it?
    repo = paste0("aux_", measure)
  )$commit$sha

  # Y drive sha ####
  # Use digest:: to get hash of the file in Y drive - should this be already saved as an attribute?







  # Y drive


}

# Get dependencies ####

#' Get dependencies of an auxiliary data measure
#'
#' This function retrieves the dependencies of a specified auxiliary data measure
#'
#' @param measure A character string specifying the auxiliary data measure
#' Must be one of the measures listed in the configuration file
#' @return A character vector of dependencies for the specified measure. If the
#' measure has no dependencies, an empty character vector is returned.
#' @keywords internal
#' @examples
#' # dependencies <- get_dependencies("cpi")
#' # print(dependencies)
#'
get_dependencies <- function(measure) {
  # Locate config file with dependencies
  yml_path <- system.file("extdata",
                          "config.yml",
                          package = "pipaux")

  if (yml_path == "") {
    stop("YAML file not found")
  }

  # Read the YAML file as a list
  yml_data <- yaml::read_yaml(yml_path)

  # Check if the measure exists in the YAML data
  if (!measure %in% names(yml_data$default)) {
    cli::cli_abort("Measure '{measure}' not found in the YAML file.")
  }

  # Extract dependencies for the given measure
  deps <- yml_data$default[[measure]]

  # If no dependencies
  if (is.null(deps) || deps == "") {
    return(character(0)) # Return empty vector for no dependencies
  }

  # Convert dependencies to a vector
  deps_vector <- unlist(strsplit(deps, ",\\s*"))

  return(deps_vector)
}

# Get file hash - TO CHECK
# get_local_file_hash <- function(filepath) {
#   if (!file.exists(filepath)) {
#     stop("Local file not found.")
#   }
#   hash <- digest::digest(file = filepath, algo = "sha256", file = TRUE)
#   return(hash)
# }
