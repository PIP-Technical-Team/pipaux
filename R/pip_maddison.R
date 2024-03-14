#' Maddison data
#'
#' Load or update data from the Maddison project.
#'
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @inheritParams pip_pfw
#' @inheritParams pipfun::load_from_gh
#' @export
#' @import data.table
pip_maddison <- function(action = c("update", "load"),
                         owner   = getOption("pipfun.ghowner"),
                         force = FALSE,
                         maindir = gls$PIP_DATA_DIR,
                         branch  = c("DEV", "PROD", "main"),
                         tag     = match.arg(branch),
                         detail  = getOption("pipaux.detail.raw")) {
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
  # validate raw data
    mpd_validate_raw(mpd = mpd, detail = detail)

  # # validate output data
  #   mpd_validate_output(mpd)

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
