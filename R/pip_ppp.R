#' PIP PPP
#'
#' Load or update PPP data.
#'
#' @inheritParams pip_prices
#' @export
#' @import data.table
pip_ppp <- function(action = "update",
                    maindir = getOption("pipaux.maindir"),
                    dlwdir  = Sys.getenv("PIP_DLW_ROOT_DIR"),
                    force = FALSE) {
  pip_prices(
    measure = "ppp",
    action = action,
    maindir = maindir,
    dlwdir = dlwdir,
    force = force
  )
}
