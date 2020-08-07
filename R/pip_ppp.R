#' PIP PPP data. Load and update
#'
#' @param action
#' @param maindir
#' @param dlwdir
#' @param force
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
pip_ppp <- function(action  = "update",
                    maindir = NULL,
                    dlwdir  = NULL,
                    force   = FALSE
){

  pip_prices(measure = "ppp",
             action  = action,
             maindir = maindir,
             dlwdir  = dlwdir,
             force   = force)

}

