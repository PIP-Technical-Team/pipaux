#' PIP GDM
#'
#' Load or update grouped data means dataset from PovcalNet Masterfile. See
#' details.
#'
#' Survey means cannot be automatically calculated for grouped data, so at some
#' stage the mean needs to be entered manually. This function reads from the PCN
#' Masterfile to ensure that PCN and PIP uses the same data means.
#'
#' The dependency on the PCN Masterfile should be changed in the future.
#'
#' @inheritParams pip_prices
#' @param pcndir character: PovcalNet Masterfile directory.
#' @export
pip_gdm <- function(action = "update",
                    force = FALSE,
                    pcndir = getOption("pipaux.pcndir"),
                    maindir = gls$PIP_DATA_DIR) {
  measure <- "gdm"

  if (action == "update") {
    pip_gdm_update(force = force, maindir = maindir, pcndir = pcndir)
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
