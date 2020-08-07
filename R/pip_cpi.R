#' PIP CPI data. Load and update
#'
#' @param action
#' @param maindir
#' @param dlwdir
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
pip_cpi <- function(action  = "update",
                    maindir = NULL,
                    dlwdir  = NULL,
                    force   = FALSE
                    ){

  pip_prices(measure = "cpi",
             action  = action,
             maindir = maindir,
             dlwdir  = dlwdir,
             force   = force)

}

