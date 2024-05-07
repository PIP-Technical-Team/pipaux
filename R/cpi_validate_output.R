#' Validate clean cpi data
#'
#' @param cpi clean cpi data, output via `pip_cpi_clean`
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#' @keywords internal
#'
#' @export
cpi_validate_output <- function(cpi, detail = getOption("pipaux.detail.output")){

  stopifnot("CPI clean data is not loaded" = !is.null(cpi))

  report <- data_validation_report()

  validate(cpi, name = "CPI output data validation") |>
    validate_if(is.character(country_code),
                description = "`country_code` should be character") |>
    validate_if(is.integer(cpi_year),
                description = "`cpi_year` should be integer") |>
    validate_if(is.numeric(survey_year),
                description = "`survey_year` should be numeric") |>
    validate_if(is.numeric(cpi),
                description = "`cpi` should be numeric") |>
    validate_if(is.numeric(ccf),
                description = "`ccf` should be numeric") |>
    validate_if(is.character(survey_acronym),
                description = "`survey_acronym` should be character") |>
    validate_if(is.numeric(change_cpi2011),
                description = "`change_cpi2011` should be numeric") |>
    validate_cols(in_set(c(0, 1)), change_cpi2011,
                  description = "`change_cpi2011` values within range") |>
    validate_if(is.character(cpi_domain),
                description = "`cpi_domain` should be character") |>
    validate_cols(in_set(c("National", "Urban/Rural")), cpi_domain,
                  description = "`cpi_domian` values within range") |>
    validate_if(is.numeric(cpi_domain_value),
                description = "`cpi_domain_value` should be numeric") |>
    validate_cols(in_set(c(0, 1)), cpi_domain_value,
                  description = "`cpi_domain_value` values within range") |>
    validate_if(is.numeric(cpi2017_unadj),
                description = "`cpi2017_unadj` should be numeric") |>
    validate_if(is.numeric(cpi2011_unadj),
                description = "`cpi2011_unadj` should be numeric") |>
    validate_if(is.numeric(cpi2011),
                description = "`cpi2011` should be numeric") |>
    validate_if(is.numeric(cpi2017),
                description = "`cpi2017` should be numeric") |>
    validate_if(is.numeric(cpi2011_SM22),
                description = "`cpi2011_SM22` should be numeric") |>
    validate_if(is.numeric(cpi2017_SM22),
                description = "`cpi2017_SM22` should be numeric") |>
    validate_cols(is.logical, cpi2005,
                  description = "`cpi2005` should be logical") |>
    validate_if(is.character(cpi_data_level),
                description = "`cpi_data_level` should be character") |>
    validate_cols(in_set(c("national", "rural", "urban")), cpi_data_level,
                  description = "`cpi_data_level` values within range") |>
    validate_if(is.numeric(cpi2011_AM23),
                description = "`cpi2011_AM23` should be numeric") |>
    validate_if(is.numeric(cpi2017_AM23),
                description = "`cpi2017_AM23` should be numeric") |>
    validate_if(is.character(cpi_id),
                description = "`cpi_id` should be character") |>
    validate_cols(not_na, country_code, cpi_year, survey_acronym, cpi_data_level,
                  description = "no missing values in key variables") |>
    validate_if(is_uniq(country_code, cpi_year, survey_acronym,
                        cpi_data_level),
                description = "no duplicate records in key variables") |>
    validate_if(is_uniq(country_code, cpi),
                description = "no duplicate cpi values") |>
    add_results(report)

  validation_record <- get_results(report, unnest = FALSE) |>
    setDT()

  if (any(validation_record[["type"]] == "error")){
    get_error_validation(validation_record, detail)
  }
}
