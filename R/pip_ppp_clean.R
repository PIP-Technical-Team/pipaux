#' Clean PPP data from datalibweb to meet PIP protocols
#'
#' @param pdefault_year numeric: default PPP year to be used.
#' @param y dataset with PPP data from datalibweb. loaded in `pip_prices()`.
#'
#' @return
#' @export
#'
#' @examples
pip_ppp_clean <- function(y, default_year = 2011) {
  x <- data.table::as.data.table(y)


  #--------- Tidy data ---------
  y <- melt(x,
            id.vars       = "code",
            measure.vars  = patterns("^ppp_[0-9]{4}.+"),
            variable.name = "ver",
            value.name    = "ppp"
  )
  y[
    ,
    c("p", "ppp_year", "release_version", "adaptation_version") := tstrsplit(ver, "_")
  ][,
    `:=`(
      p = NULL,
      ver = NULL
    )
  ]

  y[
    x,
    on = .(code),
    `:=`(
      ppp_domain     = i.ppp_domain,
      ppp_data_level = as.character(i.datalevel),
      ppp_year       = as.numeric(ppp_year)
    )
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

  ][
    , # get intersection
    `:=`(
      ppp_default         = (d1 == TRUE & d2 == TRUE & ppp_year == (default_year)),
      ppp_default_by_year = (d1 == TRUE & d2 == TRUE),
      country_code        = code
      )

  ][
    # Remove unnecessary variables
    ,
    `:=`(
      d1   = NULL,
      d2   = NULL,
      code = NULL
    )
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
  return(y)
}


