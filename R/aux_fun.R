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
