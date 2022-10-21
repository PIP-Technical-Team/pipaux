#' PIP Regions
#'
#' Update or load a dataset with regions.
#'
#' @inheritParams pip_prices
#' @inheritParams load_raw_aux
#' @export
pip_regions <- function(action  = c("update", "load"),
                        force = FALSE,
                        owner   = getOption("pipfun.ghowner"),
                        maindir = gls$PIP_DATA_DIR,
                        branch  = c("DEV", "PROD", "main"),
                        tag     = match.arg(branch)) {

  measure <- "regions"
  branch <- match.arg(branch)
  action <- match.arg(action)

  if (action == "update") {
    df <- load_raw_aux(
      measure = measure,
      owner  = owner,
      branch = branch,
      tag    = tag
    )

    msrdir <- fs::path(maindir, "_aux", branch, measure) # measure dir
    saved <- pipfun::pip_sign_save(
      x       = df,
      measure = measure,
      msrdir  = msrdir,
      force   = force
    )
    return(invisible(saved))

  } else {
    df <- load_aux(
      maindir = maindir,
      measure = measure,
      branch  = branch
    )
    return(df)

  }
}
