#' PIP Indicators
#'
#' Update or load a dataset with the indicators master sheet.
#'
#' @inheritParams pip_prices
#' @export
pip_indicators <- function(action = "update",
                           force = FALSE,
                           maindir = gls$PIP_DATA_DIR) {
  measure <- "indicators"
  msrdir <- fs::path(maindir, "_aux/", measure)

  if (action == "update") {
    df <- suppressMessages(
      readr::read_csv(fs::path(maindir, "_aux/indicators/indicators_master.csv"))
    )
    pip_sign_save(
      x = df,
      measure = measure,
      msrdir = msrdir,
      force = force
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
