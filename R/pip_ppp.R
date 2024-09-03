#' PIP PPP
#'
#' Load or update PPP data.
#'
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @inheritParams pip_pfw
#' @inheritParams pipfun::load_from_gh
#' @export
#' @import data.table
pip_ppp <- function(action = c("update", "load"),
                    maindir = gls$PIP_DATA_DIR,
                    owner   = getOption("pipfun.ghowner"),
                    branch  = c("DEV", "PROD", "main"),
                    force   = FALSE,
                    tag     = branch,
                    detail  = getOption("pipaux.detail.raw"),
                    ppp_defaults = TRUE) {

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
    pip_ppp_update(maindir = maindir,
                   force   = force,
                   owner   = owner,
                   branch  = branch,
                   tag     = tag,
                   detail  = detail)
  }
  else {
    load_aux(
      maindir = maindir,
      measure = measure,
      branch  = branch,
      ppp_defaults = ppp_defaults
    )
  }


}
