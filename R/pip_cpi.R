#' PIP CPI
#'
#' Load or update PIP CPI data.
#'
#' @param action character: Either "load" or "update". Default is "update". If
#'   "update" data will be updated on the system. If "load" data is loaded in
#'   memory.
#' @param maindir character: Main directory of project.
#' @param branch character: either "DEV" or "PROD". Refers to the branch that
#'   will be used to update either the development server or production.
#' @param tag character: specific release to be used in the update.
#' @param force logical: If TRUE data will be overwritten.
#'
#' @export
#' @import data.table
pip_cpi <- function(action = c("update", "load"),
                    maindir = gls$PIP_DATA_DIR,
                    branch  = c("DEV", "PROD", "main"),
                    force   = FALSE,
                    tag     = branch) {

  #   ____________________________________________________________________________
  #   on.exit                                                                 ####
  on.exit({

  })

  #   ____________________________________________________________________________
  #   Defenses                                                                ####
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
    pip_cpi_update(branch  = branch,
                   force   = force,
                   maindir = maindir,
                   tag     = tag)
    return(TRUE)
  }
  else {
    # load...
    return(df)
  }


}


