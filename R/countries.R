countries <- function(){
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
}
