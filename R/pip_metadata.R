#' PIP Survey Metadata
#'
#' Update or load a dataset with survey metadata.
#'
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @inheritParams pip_pfw
#' @inheritParams load_raw_indicators
#' @export
pip_metadata <- function(action  = c("update", "load"),
                         force   = FALSE,
                         owner   = getOption("pipfun.ghowner"),
                         maindir = gls$PIP_DATA_DIR,
                         branch  = c("DEV", "PROD", "main"),
                         tag     = match.arg(branch),
                         detail  = getOption("pipaux.detail.raw")) {
  measure <- "metadata"
  branch <- match.arg(branch)
  action <- match.arg(action)

  if (action == "update") {

    pip_metadata_update(
      maindir = maindir,
      force   = force,
      owner   = owner,
      branch  = branch,
      tag     = tag,
      detail  = detail
    )

  } else {

    load_aux(
      maindir = maindir,
      measure = measure,
      branch  = branch
    )

  }
}

