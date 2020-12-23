#' Load or update PCE Auxiliary data.
#'
#' @inheritParams pip_prices
#' @return
#' @export
#'
#' @examples
pip_pce <- function(action = "update",
                    force  = FALSE,
                    maindir = getOption("pipaux.maindir")) {

  measure <- "pce"
  msrdir  <- paste0(maindir, "_aux/", measure, "/")  # measure dir

  if (action == "update") {

    pip_pce_update(force = force)

  } else if (action == "load") {

    df <- load_aux(msrdir  = msrdir,
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
