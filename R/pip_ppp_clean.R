#' Clean PPP data from datalibweb to meet PIP protocols
#'
#' @param y dataset with PPP data from datalibweb. loaded in `pip_prices()`.
#' @param pppvar character: PPP variable to be used as default. Currently it is
#' "icp2011".
#'
#' @return
#' @export
#'
#' @examples
pip_ppp_clean <- function(y, pdefault_year = 2011) {
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
    c("p", "ppp_year", "rel_ver", "adap_ver") := tstrsplit(ver, "_")
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

  setorder(y, code, ppp_year, rel_ver, adap_ver)

  #--------- Get defatul version ---------

  y[ # Find Max release version
    ,
    d1 := rel_ver == max(rel_ver),
    by = .(code, ppp_year)

  ][
    # Find max adaptation version of the max release
    d1 == TRUE,
    d2 := adap_ver == max(adap_ver),
    by = .(code, ppp_year)

  ][
    , # get intersection
    ppp_default := (d1 == TRUE & d2 == TRUE & ppp_year == (default_year))
  ][
    # Remove unnecessary variables
    ,
    `:=`(
      d1 = NULL,
      d2 = NULL
    )
  ]


  y <- unique(y)  # remove duplicates
  return(y)
}


