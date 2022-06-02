#' PIP PCE
#'
#' Load or update PCE data.
#'
#' @inheritParams pip_prices
#' @param  sna_branch character: release tag of pip-sna file needed. Default is "main"
#' @export
pip_pce <- function(action     = "update",
                    force      = FALSE,
                    maindir    = gls$PIP_DATA_DIR,
                    sna_branch      = c("main", "dev")) {

  measure <- "pce"
  sna_branch <- match.arg(sna_branch)

  if (action == "update") {
    pip_pce_update(force      = force,
                   maindir    = maindir,
                   sna_branch = sna_branch)
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
