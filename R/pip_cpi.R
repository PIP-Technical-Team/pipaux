#' PIP CPI data. Load and update
#'
#' @inheritParams pip_prices
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
pip_cpi <- function(action  = "update",
                    maindir = getOption("pipaux.maindir"),
                    dlwdir  = getOption("pipaux.dlwdir"),
                    force   = FALSE
                    ){

  pip_prices(measure = "cpi",
             action  = action,
             maindir = maindir,
             dlwdir  = dlwdir,
             force   = force)

}

