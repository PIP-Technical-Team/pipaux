#' PIP Indicators
#'
#' Update or load a dataset with the indicators master sheet.
#'
#' @inheritParams pip_prices
#' @export
pip_indicators <- function(action  = c("update", "load"),
                           force   = FALSE,
                           owner   = getOption("pipfun.ghowner"),
                           maindir = gls$PIP_DATA_DIR,
                           branch  = c("DEV", "PROD", "main"),
                           tag     = match.arg(branch)) {
  measure <- "indicators"
  msrdir <- fs::path(maindir, "_aux/", measure)
  branch <- match.arg(branch)
  action <- match.arg(action)


  if (action == "update") {
    df <-
      pipfun::load_from_gh(
        measure = measure,
        owner  = owner,
        branch = branch
      )

    path <-
      glue("https://github.com/PIP-Technical-Team/aux_indicators/raw/main/indicators.csv")

    df <- suppressMessages(
      readr::read_csv(path)
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
