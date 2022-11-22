#' PIP CPI
#'
#' Load or update PIP CPI data.
#'
#' @param action character: Either "load" or "update". Default is "update". If
#'   "update" data will be updated on the system. If "load" data is loaded in
#'   memory.
#' @param maindir character: Main directory of project.
#' @param force logical: If TRUE data will be overwritten.
#' @inheritParams pipfun::load_from_gh
#'
#' @export
#' @import data.table
pip_cpi <- function(action = c("update", "load"),
                    maindir = gls$PIP_DATA_DIR,
                    force   = FALSE,
                    owner   = getOption("pipfun.ghowner"),
                    branch  = c("DEV", "PROD", "main", "old"),
                    tag     = match.arg(branch)) {

  #   ____________________________________________________________________________
  #   on.exit                                                                 ####
  on.exit({

  })

  #   ____________________________________________________________________________
  #   Defenses                                                                ####
  measure <- "cpi"
  action <- match.arg(action)
  branch <- match.arg(branch)

  stopifnot( exprs = {

  }
  )

  #   ____________________________________________________________________________
  #   Early returns                                                           ####
  if (FALSE) {
    return()
  }

  #   ____________________________________________________________________________
  #   Computations                                                            ####
  if (action == "update") {
    pip_cpi_update(maindir = maindir,
                   force   = force,
                   owner   = owner,
                   branch  = branch,
                   tag     = tag)
  }
  else {
    dt <- load_aux(
      maindir = maindir,
      measure = measure,
      branch  = branch
    )
    return(dt)
  }


}


