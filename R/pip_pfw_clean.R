#' Clean PPP data from datalibweb to meet PIP protocols
#'
#' @param y dataset with PPP data from datalibweb. loaded in `pip_prices()`.
#' @param pfw_id character: CPI and PPP ID. Extracted from `pip_cpi_update()`
#' "icp2011".
#'
#' @return
#' @export
#'
#' @examples
pip_pfw_clean <- function(y, pfw_id) {
  x <- data.table::as.data.table(y)

  old_var <- c("region", "code", "year", "ref_year", "survname")

  new_var <- c("region_code", "country_code", "surveyid_year", "reference_year", "survey_name")

  setnames(x,
           old = old_var,
           new = new_var)

  x[,
    pfw_id := (pfw_id)
    ]
  x <- unique(x)  # remove duplicates
  return(x)
}


