#' Clean PPP data from datalibweb to meet PIP protocols
#'
#' @param y dataset with PPP data from datalibweb. loaded in `pip_prices()`.
#' @param default_year numeric: ICP round year. Default is 2011
#'
#' @export
pip_ppp_clean <- function(y, default_year = getOption("pipaux.pppyear")) {

  x <- data.table::as.data.table(y)

  y <- melt(x,
            id.vars       = c("code", "ppp_domain","datalevel"),
            measure.vars  = patterns("^ppp_[0-9]{4}.+"),
            variable.name = "ver",
            value.name    = "ppp"
  )

  y[
    ,
    c("p", "ppp_year", "release_version", "adaptation_version") := tstrsplit(ver, "_")
  ][,
    `:=`(
      ppp_year   = as.numeric(ppp_year),
      ppp_domain = as.character(ppp_domain),
      datalevel  = as.character(datalevel)
    )
  ][,
    # This part should not exist if the raw data
    # has been properly created
    ppp_data_level := fcase(
      ppp_domain %chin% c("urban/rural", "2") & datalevel == "0", "rural",
      ppp_domain %chin% c("urban/rural", "2") & datalevel == "1", "urban",
      ppp_domain %chin% c("national", "1")  & datalevel %chin% c("2", "", NA_character_) , "national",
      default =  ""
    )
  ][,
    c("p", "ver", "datalevel") := NULL
  ]

  setorder(y, code, ppp_year, release_version, adaptation_version)

  #--------- Get default version ---------

  y[ # Find Max release version
    ,
    d1 := release_version == max(release_version),
    by = .(code, ppp_year)

  ][
    # Find max adaptation version of the max release
    d1 == TRUE,
    d2 := adaptation_version == max(adaptation_version),
    by = .(code, ppp_year)

  ][,
    # get intersection
    `:=`(
      ppp_default         = (d1 == TRUE & d2 == TRUE & ppp_year == (default_year)),
      ppp_default_by_year = (d1 == TRUE & d2 == TRUE),
      country_code        = code
    )

  ][,
    # Remove unnecessary variables
    c("d1", "d2", "code") := NULL
  ]

  setcolorder(y,
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

  y <- unique(y)  # remove duplicates

  # Remove non WDI countries
  non_wdi <- c('BES', 'EGZ', 'RUT', 'SDO')
  if (any(y$country_code %in% non_wdi)) {
   y <- y[!(country_code %in% non_wdi)]
  }

  return(y)
}


