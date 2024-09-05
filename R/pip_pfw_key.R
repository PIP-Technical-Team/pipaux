#' Generate a dataset that contains pfw keys
#'
#' @return data.table
#' @export
#'
pip_pfw_key <- function(){

  # pfw_temp <- pipload::pip_load_aux("pfw")

  pfw_temp <- pip_pfw(action = "load",
                 branch = "DEV",
                 maindir = "Q:/Team/Tefera/pip/PIP-Data_QA")

  pfw_key_options <- pfw_temp[, .(country_code,
                                  survey_year,
                                  survey_acronym,
                                  welfare_type,
                                  cpi_domain_var)]


  # cpi_temp <- pipload::pip_load_aux("cpi")

  cpi_temp <- pip_cpi(action = "load",
                 branch = "DEV",
                 maindir = "Q:/Team/Tefera/pip/PIP-Data_QA")

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
