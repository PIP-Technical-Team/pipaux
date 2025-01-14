#' Validate clean gdm data
#'
#' @param gdm clean gdm data, output via `pipfun::pip_gdm_clean`
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#' @keywords internal
#'
#' @export
gdm_validate_output <- function(gdm, detail = getOption("pipaux.detail.output")){

  stopifnot("GDM output data is not loaded" = !is.null(gdm))

  report <- data_validation_report()

  validate(gdm, name = "GDM output data validation") |>
    validate_if(is.character(survey_id),
                description = "`survey_id` should be character") |>
    validate_if(is.character(country_code),
                description = "`country_code` should be character") |>
    validate_if(is.integer(year),
                description = "`year` should be integer") |>
    validate_if(is.numeric(survey_year),
                description = "`survey_year` should be numeric") |>
    validate_if(is.character(welfare_type),
                description = "`welfare_type` should be character") |>
    validate_cols(in_set(c("consumption", "income")), welfare_type,
                  description = "`welfare_type` values within range") |>
    validate_if(is.numeric(survey_mean_lcu),
                description = "`survey_mean_lcu` should be numeric") |>
    validate_if(is.character(distribution_type),
                description = "`distribution_type` should be character") |>
    validate_cols(in_set(c("aggregate", "group")), distribution_type,
                  description = "`distribution_type` values within range") |>
    validate_if(is.character(gd_type),
                description = "`gd_type` should be character") |>
    validate_if(is.character(reporting_level),
                description = "`reporting_level` should be character") |>
    validate_cols(in_set(c("national", "rural", "urban")), reporting_level,
                  description = "`reporting_level` values within range") |>
    validate_if(is.character(pcn_source_file),
                description = "`pcn_source_file` should be character") |>
    validate_if(is.character(pcn_survey_id),
                description = "`pcn_survey_id` should be character") |>
    validate_cols(not_na, country_code, year, reporting_level,
                  description = "no missing values in key variables") |>
    validate_if(is_uniq(country_code, year, reporting_level),
                description = "no duplicate records in key variables") |>
    add_results(report)

  validation_record <- get_results(report, unnest = FALSE) |>
    setDT()

  if (any(validation_record[["type"]] == "error")){
    get_error_validation(validation_record, detail)
  }
}
