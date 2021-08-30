#' PIP PFW
#'
#' Load or update PIP Price Framework data.
#'
#' @inheritParams pip_prices
#' @export
#' @import data.table
pip_pfw <- function(action = "update",
                    maindir = gls$PIP_DATA_DIR,
                    dlwdir  = Sys.getenv("PIP_DLW_ROOT_DIR"),
                    force = FALSE) {
  pip_prices(
    measure = "pfw",
    action = action,
    maindir = maindir,
    dlwdir = dlwdir,
    force = force
  )
}
