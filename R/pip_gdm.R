#' PIP GDM
#'
#' Load or update grouped data means dataset from PovcalNet Masterfile. See
#' details.
#'
#' Survey means cannot be automatically calculated for grouped data, so at some
#' stage the mean needs to be entered manually. This function reads from the PCN
#' Masterfile to ensure that PCN and PIP uses the same data means.
#'
#' The dependency on the PCN Masterfile should be changed in the future.
#'
#' @inheritParams pip_cpi
#' @inheritParams pipfun::load_from_gh
#' @export
pip_gdm <- function(action  = c("update", "load"),
                    force   = FALSE,
                    owner   = getOption("pipfun.ghowner"),
                    maindir = gls$PIP_DATA_DIR,
                    branch  = c("DEV", "PROD", "main"),
                    tag     = match.arg(branch)) {

  measure <- "gdm"
  branch <- match.arg(branch)
  action <- match.arg(action)

  if (action == "update") {

    pip_gdm_update(force   = force,
                   maindir = maindir,
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
