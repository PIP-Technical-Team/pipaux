#' PIP Indicators
#'
#' Update or load a dataset with the indicators master sheet.
#'
#' @inheritParams pip_pfw
#' @inheritParams load_raw_aux
#' @export
pip_indicators <- function(action  = c("update", "load"),
                           force   = FALSE,
                           owner   = getOption("pipfun.ghowner"),
                           maindir = gls$PIP_DATA_DIR,
                           branch  = c("DEV", "PROD", "main"),
                           tag     = match.arg(branch)) {

  measure <- "indicators"
  branch <- match.arg(branch)
  action <- match.arg(action)

  if (action == "update") {

    df <- load_raw_aux(measure = measure,
                       owner = owner,
                       branch = branch)

    # Save dataset
    msrdir <- fs::path(maindir, "_aux", branch, measure) # measure dir
    saved <- pip_sign_save(
      x       = df,
      measure = measure,
      msrdir  = msrdir,
      force   = force
    )
    return(invisible(saved))

  } else  {

    dt <- load_aux(
      maindir = maindir,
      measure = measure,
      branch  = branch
    )
    return(dt)

  }
}
