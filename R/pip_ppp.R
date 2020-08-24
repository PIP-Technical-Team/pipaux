#' PIP PPP data. Load and update
#'
#' @param action character: Either "load" or "update". Default is "update". If
#' "update" PPP data will be updated in system. If "load" PPP data is loaded in memory
#' @param maindir character: main directory of project. Default available
#' in `pip_aux_values()`
#' @param dlwdir character: Datalibweb directory available in `pip_aux_values()`
#' @param force logical: If TRUE PPP data will be updated.
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
pip_ppp <- function(action  = "update",
                    maindir = NULL,
                    dlwdir  = getOption("pipaux.dlwdir"),
                    force   = FALSE
){

  pip_prices(measure = "ppp",
             action  = action,
             maindir = maindir,
             dlwdir  = dlwdir,
             force   = force)

}

