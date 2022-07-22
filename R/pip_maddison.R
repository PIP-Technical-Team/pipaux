#' Maddison data
#'
#' Load or update data from the Maddison project.
#'
#' @inheritParams pip_pfw
#' @inheritParams load_raw_aux
#' @export
#' @import data.table
pip_maddison <- function(action = c("update", "load"),
                         owner   = getOption("pipaux.ghowner"),
                         force = FALSE,
                         maindir = gls$PIP_DATA_DIR,
                         branch  = c("DEV", "PROD", "main"),
                         tag     = match.arg(branch)) {
  measure <- "maddison"
  action  <- match.arg(action)
  branch  <- match.arg(branch)

  if (action == "update") {
    mpd <-  load_raw_aux(
      measure = measure,
      owner  = owner,
      branch = branch,
      tag    = tag
    )

    msrdir <- fs::path(maindir, "_aux", branch, measure) # measure dir
    saved <- pip_sign_save(
      x = mpd,
      measure = measure,
      msrdir = msrdir,
      force = force
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
