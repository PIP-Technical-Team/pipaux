#' PIP CPI data. Load and update
#'
#' @inheritParams pip_prices
#'
#' @export
#' @import data.table
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

