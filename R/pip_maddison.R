#' Maddison data
#'
#' Load or update data from the Maddison project.
#'
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
  msrdir <- fs::path(maindir, "_aux/", measure) # measure dir

  if (action == "update") {
    mpd <-  load_raw_aux(
      measure = measure,
      owner  = owner,
      branch = branch,
      tag    = tag
    )

    pip_sign_save(
      x = mpd,
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
    rlang::abort(c("`action` must be `update` or `load`",
      x = paste0("you provided `", action, "`")
    ))
  }
}
