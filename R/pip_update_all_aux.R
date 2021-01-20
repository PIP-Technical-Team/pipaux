#' Update all auxiliary data at once
#'
#' @inheritParams pip_prices
#' @export
pip_update_all_aux <- function(force = FALSE,
                               maindir = getOption("pipaux.maindir")) {
  pip_pfw(force = force, maindir = maindir)
  pip_cpi(force = force, maindir = maindir)
  pip_gdp(force = force, maindir = maindir)
  pip_pce(force = force, maindir = maindir)
  pip_pop(force = force, maindir = maindir)
  pip_ppp(force = force, maindir = maindir)
  pip_maddison(force = force, maindir = maindir)
  pip_country_list(force = force, maindir = maindir)

  return(invisible())
}
