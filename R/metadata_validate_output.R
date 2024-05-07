#' Validate output metadata data
#'
#' @param metadata metadata data
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#' @keywords internal
#'
#' @export
metadata_validate_output <- function(metadata, detail = getOption("pipaux.detail.output")){

  stopifnot("Metadata data is not loaded" = !is.null(metadata))

  report <- data_validation_report()

  validate(metadata, name = "Metadata output data validation") |>
    validate_if(is.character(country_code),
                description = "`country_code` should be character") |>
    validate_if(is.character(country_name),
                description = "`country_name` should be character") |>
    validate_if(is.numeric(reporting_year),
                description = "`reporting_year` should be numeric") |>
    validate_if(is.numeric(survey_year),
                description = "`survey_year` should be numeric") |>
    validate_if(is.character(survey_title),
                description = "`survey_title` should be character") |>
    validate_if(is.character(survey_conductor),
                description = "`survey_conductor` should be character") |>
    validate_if(is.character(survey_coverage),
                description = "`survey_coverage` should be character") |>
    validate_cols(in_set(c("national", "rural", "urban")),
                  survey_coverage, description = "`survey_coverage` values within range") |>
    validate_if(is.character(welfare_type),
                description = "`welfare_type` should be character") |>
    validate_cols(in_set(c("consumption", "income")),
                  welfare_type, description = "`welfare_type` values within range") |>
    validate_if(is.character(distribution_type),
                description = "`distribution_type` should be character") |>
    validate_cols(in_set(c("aggregated", "group", "micro", "micro, imputed", NA)),
                  distribution_type, description = "`distribution_type` values within range") |>
    validate_cols(not_na, country_code, reporting_year, welfare_type,
                  description = "no missing values in key variables") |>
    validate_if(is_uniq(country_code, reporting_year, welfare_type),
                description = "no duplicate records in key variables") |>
    add_results(report)

  validation_record <- get_results(report, unnest = FALSE) |>
    setDT()

  if (any(validation_record[["type"]] == "error")){
    get_error_validation(validation_record, detail)
  }

}
