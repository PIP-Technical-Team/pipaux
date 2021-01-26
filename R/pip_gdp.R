#' PIP GDP
#'
#' Update or load GDP data.
#'
#' @inheritParams pip_prices
#' @param maddison_action character: Either "load" or "update". Default is "update".
#' @param weo_action character: Either "load" or "update". Default is "update".
#' @export
#' @import data.table
pip_gdp <- function(action = "update",
                    maddison_action = "load",
                    weo_action = "load",
                    force = FALSE,
                    maindir = getOption("pipaux.maindir")) {


  measure   <- "gdp"

  # Update Maddison Project Data
  if (maddison_action == "update") {
    pip_maddison(force = force, maindir = maindir)
  }

  # Update WEO data
  if (weo_action == "update") {
    pip_gdp_weo(force = force, maindir = maindir)
  }

  if (action == "update") {

    pip_gdp_update(force = force, maindir = maindir)

  } else if (action == "load") {
    load_aux(maindir = maindir,
             measure = measure)

  }  # End of update

} # end of pip_gdp
