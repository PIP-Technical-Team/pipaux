#' Country Profiles
#'
#' Update a list with country profiles data
#'
#' @inheritParams pip_prices
#' @export
#'
pip_cp <- function(action = "update",
                   force = FALSE,
                   maindir = getOption("pipaux.maindir")) {
  if (action == "update") {
    pip_cp_update(force = force, maindir = maindir)
  } else if (action == "load") {
    dl <- readRDS(paste0(maindir, "_aux/cp/cp.rds"))
  }
}
