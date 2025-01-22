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

  get_deps <- get_dependencies(measure)

}

# Get dependencies ####
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
    cli::cli_abort("measure {measure} not found")
  }

  # Extract dependencies for the given measure
  deps <- yml_data$default[[measure]]

  # If no dependencies
  if (is.null(deps) || deps == "") {
    return(character(0)) # Return empty vector for no dependencies
  }

  # Convert dependencies to a vector
  deps_vector <- unlist(strsplit(deps,
                                 ",\\s*"))

  return(deps_vector)
}

