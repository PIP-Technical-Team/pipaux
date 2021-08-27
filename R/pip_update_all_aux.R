#' Update all auxiliary data at once
#'
#' @inheritParams pip_prices
#' @param popsrc character: Source for population data. Defaults to `getOption("pipaux.popsrc")`.
#' @export
pip_update_all_aux <- function(force = FALSE,
                               popsrc = getOption("pipaux.popsrc"),
                               maindir = getOption("pipaux.maindir")) {

  # List of countries in WDI
  pip_country_list(force = force, maindir = maindir)

  # PIP countries and regions
  pip_countries(force = force, maindir = maindir)
  pip_regions(force = force, maindir = maindir)

  # PIP Indicators
  pip_indicators(force = force, maindir = maindir)

  # Poverty lines
  pip_pl(force = force, maindir = maindir)

  # PFW, CPI and PPP from DLW
  pip_pfw(force = force, maindir = maindir)
  pip_cpi(force = force, maindir = maindir)
  pip_ppp(force = force, maindir = maindir)

  # POP from Emi or WDI
  pip_pop(force = force, maindir = maindir, src = popsrc)

  # GDP from WEO, Maddison and WDI (+ a few special cases)
  pip_gdp_weo(force = force, maindir = maindir)
  pip_maddison(force = force, maindir = maindir)
  pip_gdp(force = force, maindir = maindir)

  # PCE from WDI (+ a few special cases)
  pip_pce(force = force, maindir = maindir)

  # Country profiles (from Poverty GP)
  pip_cp(force = force, maindir = maindir)

  # Survey metadata (from Poverty GP)
  pip_metadata(force = force, maindir = maindir)

  return(invisible())
}
