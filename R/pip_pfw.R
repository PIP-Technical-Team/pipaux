#' PIP PFW
#'
#' Load or update PIP Price Framework data.
#'
#' @param action character: Either "load" or "update". Default is "update". If
#' "update" data will be updated on the system. If "load" data is loaded in memory.
#' @param maindir character: Main directory of project.
#' @param force logical: If TRUE data will be overwritten.
#' @inheritParams pipfun::load_from_gh
#' @export
#' @import data.table
pip_pfw <- function(action  = c("update", "load"),
                    force   = FALSE,
                    owner   = getOption("pipfun.ghowner"),
                    maindir = gls$PIP_DATA_DIR,
                    branch  = c("DEV", "PROD", "main"),
                    tag     = match.arg(branch)) {
  measure <- "pfw"
  branch <- match.arg(branch)
  action <- match.arg(action)

  if (action == "update") {
    pip_pfw_update(maindir = maindir,
                   force   = force,
                   owner   = owner,
                   branch  = branch,
                   tag     = tag)

  } else {

    dt <- load_aux(
      maindir = maindir,
      measure = measure,
      branch  = branch
    )
    return(dt)
  }
}
