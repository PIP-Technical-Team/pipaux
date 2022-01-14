#' Transform PPP
#'
#' @param df data.frame: PPP data from Datalibweb.
#' @return data.table
#' @keywords internal
transform_ppp <- function(df) {

  dt <- data.table::as.data.table(df)

  dt <- melt(dt,
            id.vars       = c("code", "ppp_domain", "datalevel"),
            measure.vars  = patterns("^ppp_[0-9]{4}.+"),
            variable.name = "ver",
            value.name    = "ppp"
  )

  dt[
    ,
    c("p", "ppp_year", "release_version", "adaptation_version") := tstrsplit(ver, "_")
  ][
    ,
    `:=`(
      ppp_year   = as.numeric(ppp_year),
      ppp_domain = as.character(ppp_domain),
      datalevel  = as.character(datalevel)
    )
  ][
    ,
    # This part should not exist if the raw data
    # has been properly created
    ppp_data_level := fcase(
      ppp_domain %chin% c("urban/rural", "2") & datalevel == "0", "rural",
      ppp_domain %chin% c("urban/rural", "2") & datalevel == "1", "urban",
      ppp_domain %chin% c("national", "1") & datalevel %chin% c("2", "", NA_character_), "national",
      default = ""
    )
  ][
    ,
    c("p", "ver", "datalevel") := NULL
  ]

  setorder(dt, code, ppp_year, release_version, adaptation_version)

  #--------- Get default version ---------

  dt[ # Find Max release version
    ,
    d1 := release_version == max(release_version),
    by = .(code, ppp_year)
  ][
    # Find max adaptation version of the max release
    d1 == TRUE,
    d2 := adaptation_version == max(adaptation_version),
    by = .(code, ppp_year)
  ][
    ,
    # get intersection
    `:=`(
      ppp_default         = (d1 == TRUE & d2 == TRUE & ppp_year == (default_year)),
      ppp_default_by_year = (d1 == TRUE & d2 == TRUE),
      country_code        = code
    )
  ][
    ,
    # Remove unnecessary variables
    c("d1", "d2", "code") := NULL
  ]

  setcolorder(
    dt,
    c(
      "country_code",
      "ppp_year",
      "release_version",
      "adaptation_version",
      "ppp",
      "ppp_default",
      "ppp_default_by_year",
      "ppp_domain",
      "ppp_data_level"
    )
  )

  dt <- unique(dt) # remove duplicates

  # Remove non WDI countries
  non_wdi <- c("BES", "EGZ", "RUT", "SDO")
  if (any(dt$country_code %in% non_wdi)) {
    dt <- dt[!(country_code %in% non_wdi)]
  }

  # Remove any non-WDI countries
  #ppp <- ppp[country_code %in% cl$country_code]

  # Hardcode domain / data_level fix for NRU
  dt$ppp_domain <-
    ifelse(dt$country_code == "NRU" & is.na(dt$ppp_domain),
           1, dt$ppp_domain
    )
  dt$ppp_data_level <-
    ifelse(dt$country_code == "NRU" & dt$ppp_data_level == "",
           "national", dt$ppp_data_level
    )


  return(dt)
}


