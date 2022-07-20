#' PIP Dictionary
#'
#' Update or load a dataset with the indicators master sheet.
#'
#' @inheritParams pip_pfw
#' @inheritParams load_raw_aux
#' @export
pip_dictionary <- function(action  = c("update", "load"),
                           force   = FALSE,
                           owner   = getOption("pipaux.ghowner"),
                           maindir = gls$PIP_DATA_DIR,
                           branch  = c("DEV", "PROD", "main"),
                           tag     = match.arg(branch)) {
  measure <- "dictionary"
  branch <- match.arg(branch)
  action <- match.arg(action)

  if (action == "update") {

    df <- load_raw_aux(measure = measure,
                       owner = owner,
                       branch = branch,
                       tag = tag)
    # Save dataset
    msrdir <- fs::path(maindir, "_aux", branch, measure) # measure dir
    saved <- pip_sign_save(
      x       = df,
      measure = measure,
      msrdir  = msrdir,
      force   = force
    )

    return(invisible(saved))

  } else {
    load_aux(
      maindir = maindir,
      measure = measure,
      branch  = branch
    )
  }
}
