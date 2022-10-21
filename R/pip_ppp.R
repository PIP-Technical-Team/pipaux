#' PIP PPP
#'
#' Load or update PPP data.
#'
#' @inheritParams pip_pfw
#' @inheritParams pipfun::load_from_gh
#' @export
#' @import data.table
pip_ppp <- function(action = c("update", "load"),
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
  measure <- "ppp"
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
    pip_ppp_update(branch  = branch,
                   force   = force,
                   maindir = maindir,
                   tag     = tag)
  }
  else {
    load_aux(
      maindir = maindir,
      measure = measure,
      branch  = branch
    )
  }


}
