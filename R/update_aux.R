#' Update Auxiliary data. Wrapper of measure-specific functions.
#'
#' @inheritParams pip_prices
#' @param ... Arguments of any of the pip_* functions for updating data.
#'
#' @return
#' @export
#'
#' @examples
update_aux <- function(measure  = NULL,
                      ...
                      ){

  # verify measure is provided
  if (is.null(measure)) {

    rlang::abort(c(
      "`measure` must be defined, as it does not have default value",
      i = "make sure `measure` is not NULL."
    ),
    class = "pipaux_error"
    )

  }

  # check arguments
  al <- list(...)  # Arguments List
  an <- names(al)  # arguments names

  if (!any(an == "maindir")) {
    al["maindir"] <- getOption("pipaux.maindir")
  }


  # build function name
  fun_name <- get(paste0("pip_", measure))

  rs <- do.call(fun_name, c(action = "update", al))
  return(invisible(rs))

}

