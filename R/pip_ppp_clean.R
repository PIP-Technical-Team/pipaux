#' Clean PPP data from datalibweb to meet PIP protocols
#'
#' @param y dataset with PPP data from datalibweb. loaded in `pip_prices()`
#'
#' @return
#' @export
#'
#' @examples
pip_ppp_clean <- function(y) {
  x <- data.table::as.data.table(y)

  # vars to keep
  keep_vars <- c("country_code", "surveyid_year", "reference_year",
                 "ppp2011", "datalevel")

  # modifications to the database
  x[
    ,
    `:=`(
      country_code   =  code,
      surveyid_year  = year,
      reference_year = ref_year,
      ppp2011        = icp2011

    )
  ]

  # keep final vars
  x <- x[
    ,
    ..keep_vars
  ]
  return(x)
}


