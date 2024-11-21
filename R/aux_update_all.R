#' Update all auxiliary data at once
#'
#' @inheritParams aux_cpi
#' @param popsrc character: Source for population data. Defaults to `getOption("pipaux.popsrc")`.
#' @export
aux_update_all <- function(force = FALSE,
                               popsrc = getOption("pipaux.popsrc"),
                               maindir = gls$PIP_DATA_DIR) {

  # List of countries in WDI
  aux_country_list(force = force, maindir = maindir)

  # PIP countries and regions
  aux_countries(force = force, maindir = maindir)
  aux_regions(force = force, maindir = maindir)

  # PIP Indicators
  aux_indicators(force = force, maindir = maindir)

  # Poverty lines
  aux_pl(force = force, maindir = maindir)

  # PFW, CPI and PPP from DLW
  aux_pfw(force = force, maindir = maindir)
  aux_cpi(force = force, maindir = maindir)
  aux_ppp(force = force, maindir = maindir)

  # POP from Emi or WDI
  aux_pop(force = force, maindir = maindir, src = popsrc)

  # GDP from WEO, Maddison and WDI (+ a few special cases)
  aux_weo(force = force, maindir = maindir)
  aux_maddison(force = force, maindir = maindir)
  aux_gdp(force = force, maindir = maindir)

  # PCE from WDI (+ a few special cases)
  aux_pce(force = force, maindir = maindir)

  # Country profiles (from Poverty GP)
  aux_cp(force = force, maindir = maindir)

  # Survey metadata (from Poverty GP)
  aux_metadata(force = force, maindir = maindir)

  return(invisible())
}
