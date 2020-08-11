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
pip_pfw_clean <- function(y, pppvar = "icp2011") {
  x <- data.table::as.data.table(y)

  old_var <- c("region", "code", "year", "ref_year", "survname")

  new_var <- c("region_code", "country_code", "surveyid_year", "reference_year", "survey_name")

  setnames(x,
           old = old_var,
           new = new_var)

  # # vars to keep
  # drop_vars <- c("region_code", "country_code", "surveyid_year", "reference_year",
  #                "ppp", "ppp_domain", "ppp_data_level")
  #
  # # modifications to the database
  # x[
  #   ,
  #   `:=`(
  #     region_code    = region,
  #     country_code   = code,
  #     surveyid_year  = year,
  #     reference_year = ref_year,
  #     ppp            = get(pppvar)
  #   )
  # ]
  #
  # # keep final vars
  # x <- x[
  #         ,
  #         !..drop_vars
  #       ]

  x <- unique(x)  # remove duplicates
  return(x)
}


