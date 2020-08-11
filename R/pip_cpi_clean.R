#' Clean CPI data from datalibweb to meet PIP protocols
#'
#' @param y dataset with CPI data from datalibweb. loaded in `pip_prices()`.
#' @param cpivar character: CPI variable to be used as default. Currently it is
#' "cpi2011".
#'
#' @return
#' @export
#'
#' @examples
pip_cpi_clean <- function(y, cpivar = "cpi2011") {

  x <- data.table::as.data.table(y)

  # vars to keep
  keep_vars <- c("country_code", "surveyid_year", "reference_year",
                 "cpi", "ccf", "cpi_domain", "cpi_data_level")

  # modifications to the database
  x[,
    c("cur_adj", "ccf")
    := {

      cur_adj      <-  ifelse(is.na(cur_adj), 1, cur_adj)
      ccf          <-  1/cur_adj

      list(cur_adj, ccf)
    }
  ][
    ,
    `:=`(
      country_code   = code,
      surveyid_year  = year,
      reference_year = ref_year,
      cpi            = get(cpivar)
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


