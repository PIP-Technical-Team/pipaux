#' Country Profiles
#'
#' Update a list with country profiles data
#'
#' @inheritParams pip_pfw
#' @inheritParams load_raw_aux
#' @export
pip_cp <- function(action  = c("update", "load"),
                   force   = FALSE,
                   owner   = getOption("pipaux.ghowner"),
                   maindir = gls$PIP_DATA_DIR,
                   branch  = c("DEV", "PROD", "main"),
                   tag     = match.arg(branch)) {
  measure <- "cp"
  branch <- match.arg(branch)
  action <- match.arg(action)

  if (action == "update") {
    pip_cp_update(maindir = maindir,
                  force   = force,
                  owner   = owner,
                  branch  = branch,
                  tag     = tag)
  } else {

    dl <- load_aux(
      maindir = maindir,
      measure = measure,
      branch  = branch
    )
    return(dl)
  }
}
