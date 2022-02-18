#' PIP Countries
#'
#' Update or load a dataset with countries.
#'
#' @inheritParams pip_prices
#' @export
pip_countries <- function(action = "update",
                          force = FALSE,
                          maindir = gls$PIP_DATA_DIR) {
  measure <- "countries"
  msrdir <- fs::path(maindir, "_aux/", measure)

  if (action == "update") {
    wdi_countries <- wbstats::wb_countries(lang = "en")
    data.table::setDT(wdi_countries)
    wdi_countries <- wdi_countries[region != "Aggregates"]

    pfw <- load_aux("pfw", maindir = maindir)
    pfw <- pfw[pfw$inpovcal == 1, ]
    pfw <- pfw[, c("country_code", "pcn_region_code", "wb_region_code")]
    pfw <- unique(pfw)

    countries <- wdi_countries[wdi_countries$iso3c %in% pfw$country_code, ]
    countries <- merge(countries, pfw,
      all.x = TRUE,
      by.x = "iso3c", by.y = "country_code"
    )

    countries <- countries[, c(
      "pcn_region_code", "iso3c",
      "country", "income_level",
      "iso2c"
    )]
    names(countries) <- c(
      "pcn_region_code",
      "country_code",
      "country_name",
      "income_group",
      "iso2_code"
    )

    pip_sign_save(
      x = countries,
      measure = measure,
      msrdir = msrdir,
      force = force
    )
  } else if (action == "load") {
    df <- load_aux(
      maindir = maindir,
      measure = measure
    )
    return(df)
  } else {
    msg <- paste("action `", action, "` is not a valid action.")
    rlang::abort(c(
      msg,
      i = "make sure you select `update` or `load`"
    ),
    class = "pipaux_error"
    )
  }
}
