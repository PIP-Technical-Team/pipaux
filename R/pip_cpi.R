#' PIP CPI
#'
#' Load or update PIP CPI data.
#'
#' @inheritParams pip_prices
#' @export
#' @import data.table
pip_cpi <- function(action = "update",
                    maindir = gls$PIP_DATA_DIR,
                    dlwdir  = Sys.getenv("PIP_DLW_ROOT_DIR"),
                    force = FALSE) {
  pip_prices(
    measure = "cpi",
    action = action,
    maindir = maindir,
    dlwdir = dlwdir,
    force = force
  )
}
