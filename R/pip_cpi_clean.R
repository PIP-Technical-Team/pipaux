#' Title
#'
#' @param y
#'
#' @return
#' @export
#'
#' @examples
pip_cpi_clean <- function(y) {
  x <- data.table::as.data.table(y)

  # vars to keep
  keep_vars <- c("country_code", "surveyid_year", "reference_year",
                 "cpi2011", "ccf", "datalevel")

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
      country_code   =  code,
      surveyid_year  = year,
      reference_year = ref_year

    )
  ]

  # keep final vars
  x <- x[
    ,
    ..keep_vars
  ]

  # Label variables
  attr(x$ccf, "label")            <- "Currency conversion factor"
  attr(x$reference_year, "label") <- "Proportion of first year of survey"
  attr(x$datalevel, "label")      <- "Data level to join with microdata"
  attr(x$surveyid_year, "label")  <- "Survey ID year"

  return(x)
}


