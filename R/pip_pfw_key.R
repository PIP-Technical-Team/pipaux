#' Generate a dataset that contains pfw keys
#'
#' @return data.table
#' @export
#'
pip_pfw_key <- function(){

  pfw_temp <- load_aux("pfw", maindir = temp_fld)

  pfw_key_options <- pfw_temp[, .(country_code,
                                  survey_year,
                                  survey_acronym,
                                  welfare_type,
                                  cpi_domain_var)]


  cpi_temp <- load_aux("cpi", maindir = temp_fld)

  cpi_temp <- cpi_temp[, cpi_domain_var :=
                         fifelse(reporting_level == "urban" &
                                   cpi_domain_value == 1, "urban", "")]

  cpi_temp <- cpi_temp[, .(country_code, survey_year, survey_acronym,
                           cpi_domain_var, reporting_level)]

  pfw_key <- cpi_temp[pfw_key_options, on = .(country_code, survey_year,
                                             survey_acronym, cpi_domain_var)]

  any(duplicated(pfw_key, by = c("country_code", "survey_year", "survey_acronym", "cpi_domain_var")))

  return(pfw_key)
}
