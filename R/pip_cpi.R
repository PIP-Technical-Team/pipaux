#' PIP CPI data. Load and update
#'
#' @param action character: Either "load" or "update". Default is "update". If
#' "update" CPI data will be updated in system. If "load" CPI data is loaded in memory
#' @param maindir character: main directory of project. Default available
#' in `pip_aux_values()`
#' @param dlwdir character: Datalibweb directory available in getOptions("pipaux.dlwdir")
#' @param force logical: If TRUE CPI data will be updated.
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
pip_cpi <- function(action  = "update",
                    maindir = NULL,
                    dlwdir  = getOption("pipaux.dlwdir"),
                    force   = FALSE
                    ){

  pip_prices(measure = "cpi",
             action  = action,
             maindir = maindir,
             dlwdir  = dlwdir,
             force   = force)

}

