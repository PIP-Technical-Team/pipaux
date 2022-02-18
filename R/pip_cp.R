#' Country Profiles
#'
#' Update a list with country profiles data
#'
#' @inheritParams pip_prices
#' @export
pip_cp <- function(action = "update",
                   force = FALSE,
                   maindir = gls$PIP_DATA_DIR) {
  if (action == "update") {
    pip_cp_update(force = force, maindir = maindir)
  } else if (action == "load") {
    dl <- readRDS(fs::path(maindir, "_aux/cp/cp.rds"))
  }
}
