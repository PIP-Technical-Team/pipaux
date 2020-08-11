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
pip_ppp_clean <- function(y, pppvar = "icp2011") {
  x <- data.table::as.data.table(y)

  # vars to keep
  keep_vars <- c("country_code", "surveyid_year", "reference_year",
                 "ppp", "ppp_domain", "ppp_data_level")

  # modifications to the database
  x[
    ,
    `:=`(
      country_code   = code,
      surveyid_year  = year,
      reference_year = ref_year,
      ppp            = get(pppvar)
    )
  ]

  # keep final vars
  x <- x[
    ,
    ..keep_vars
  ]

  x <- unique(x)  # remove duplicates
  return(x)
}


