#' Maddison data
#'
#' Load or update data from the Maddison project.
#'
#' @inheritParams pip_pfw
#' @inheritParams pipfun::load_from_gh
#' @export
#' @import data.table
pip_maddison <- function(action = c("update", "load"),
                         owner   = getOption("pipfun.ghowner"),
                         force = FALSE,
                         maindir = gls$PIP_DATA_DIR,
                         branch  = c("DEV", "PROD", "main", "test"),
                         tag     = match.arg(branch)) {
  measure <- "maddison"
  action  <- match.arg(action)
  branch  <- match.arg(branch)

  if (action == "update") {
    mpd <-  pipfun::load_from_gh(
      measure = measure,
      owner  = owner,
      branch = branch,
      tag    = tag
    )

  if (branch == "main") {
    branch <- ""
  }
  msrdir <- fs::path(maindir, "_aux", branch, measure) # measure dir
    saved <- pipfun::pip_sign_save(
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
