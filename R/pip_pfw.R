#' Price framework Data. Load and update
#'
#' @inheritParams pip_prices
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
pip_pfw <- function(action  = "update",
                    maindir = getOption("pipaux.maindir"),
                    dlwdir  = getOption("pipaux.dlwdir"),
                    force   = FALSE
){

  pip_prices(measure = "pfw",
             action  = action,
             maindir = maindir,
             dlwdir  = dlwdir,
             force   = force)

}

