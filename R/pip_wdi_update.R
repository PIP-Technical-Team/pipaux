#' Update National accounts data from WDI
#'
#' GDP and HFCE data from WDI. It could be either from API or from file
#'
#' @inheritParams pip_gdp
#' @param from character: Either "file" or "api". Default is file.
#'
#' @return data.table with gdp and pce variables
#' @export
#'
#' @examples
pip_wdi_update <- function(force = FALSE,
                           maindir = gls$PIP_DATA_DIR,
                           from = c("file", "api")) {

  #   ____________________________________________________________________________
  #   on.exit                                                                 ####
  on.exit({

  })

  #   ____________________________________________________________________________
  #   Defenses                                                                ####
  from <- match.arg(from)
  stopifnot( exprs = {

  }
  )

  #   ____________________________________________________________________________
  #   Early returns                                                           ####

  if (FALSE) {
    return()
  }

  #   ____________________________________________________________________________
  #   Computations                                                            ####

  wdidir <- fs::path(maindir, "_aux/wdi")


  ##  ............................................................................
  ##  From file                                                               ####

  if (from == "file") {

  }


  ##  ............................................................................
  ##  From API                                                                ####


  #   ____________________________________________________________________________
  #   Return                                                                  ####

  return(wdi)

}


