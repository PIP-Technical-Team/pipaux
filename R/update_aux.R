#' Update Auxiliary data. Wrapper of measure-specific functions.
#'
#' @inheritParams pip_cpi
#' @param ... Arguments of any of the pip_* functions for updating data.
#' @export
update_aux <- function(measure = NULL,
                       ...) {

  # verify measure is provided
  if (is.null(measure)) {
    msg     <- c(
      "{.field measure} must be defined, as it does not have default value",
      "i" = "make sure `measure` is not NULL."
      )
    cli::cli_abort(msg,
                  class = "pipaux_error"
                  )
  }

  # check arguments
  al <- list(...) # Arguments List
  an <- names(al) # arguments names

  if (!any(an == "maindir")) {
    al["maindir"] <- gls$PIP_DATA_DIR
  }


  # build function name
  fun_name <- get(paste0("pip_", measure))

  rs <- do.call(fun_name, c(action = "update", al))
  return(invisible(rs))
}
