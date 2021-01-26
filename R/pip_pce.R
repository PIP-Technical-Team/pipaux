#' PIP PCE
#'
#' Load or update PCE data.
#'
#' @inheritParams pip_prices
#' @export
pip_pce <- function(action = "update",
                    force  = FALSE,
                    maindir = getOption("pipaux.maindir")) {

  measure <- "pce"

  if (action == "update") {

    pip_pce_update(force = force, maindir = maindir)

  } else if (action == "load") {

    df <- load_aux(maindir  = maindir,
                   measure = measure)
    return(df)


  } else {
    msg <- paste("action `", action,"` is not a valid action.")
    rlang::abort(c(
      msg,
      i = "make sure you select `update` or `load`"
    ),
    class = "pipaux_error"
    )
  }

}
