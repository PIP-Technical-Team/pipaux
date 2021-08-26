#' PIP POP
#'
#' Load or update population data.
#'
#' @inheritParams pip_prices
#' @param src character: Source for population data. Defaults to `getOption("pipaux.popsrc")`.
#' @export
pip_pop <- function(action = "update",
                    force = FALSE,
                    src = getOption("pipaux.popsrc"),
                    maindir = getOption("pipaux.maindir")) {
  measure <- "pop"
  src <- tolower(src)

  if (action == "update") {
    pip_pop_update(
      force = force,
      src = src,
      maindir = maindir
    )
  } else if (action == "load") {
    df <- load_aux(
      maindir = maindir,
      measure = measure
    )
    return(df)
  } else {
    msg <- paste("action `", action, "` is not a valid action.")
    rlang::abort(c(
      msg,
      i = "make sure you select `update` or `load`"
    ),
    class = "pipaux_error"
    )
  }
}
