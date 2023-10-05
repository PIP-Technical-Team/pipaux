#' PIP Dictionary
#'
#' Update or load a dataset with the indicators master sheet.
#'
#' @inheritParams pip_prices
#' @export
pip_dictionary <- function(action = "update",
                           force = FALSE,
                           maindir = gls$PIP_DATA_DIR) {
  measure <- "dictionary"
  msrdir <- fs::path(maindir, "_aux/", measure)

  if (action == "update") {
    u <- "https://github.com/PIP-Technical-Team/variable_diccionary/blob/main/Variable%20Diccionary.csv?raw=true"
    df <- suppressMessages(
      readr::read_csv(u)
    )
    # df <- df[order(df$variable),]
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
