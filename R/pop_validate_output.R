#' Validate output pop data
#'
#' @param pop output pop data
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#' @keywords internal
#'
#' @export
pop_validate_output <- function(pop, detail = getOption("pipaux.detail.output")){

  stopifnot("POP clean data is not loaded" = !is.null(pop))

  report <- data_validation_report()

  validate(pop, name = "POP output data validation") |>
    validate_if(is.character(country_code),
                description = "`country_code` should be character") |>
    validate_if(is.numeric(year),
                description = "`year` should be numeric") |>
    validate_if(is.character(reporting_level),
                description = "`reporting_level` should be character") |>
    validate_cols(in_set(c("national", "rural", "urban")),
                  reporting_level, description = "`reporting_level` values within range") |>
    validate_if(is.numeric(pop),
                description = "`pop` should be numeric") |>
    validate_if(is.character(pop_domain),
                description = "`pop_domain` should be character") |>
    validate_cols(in_set(c("national", "urban/rural")),
                  pop_domain, description = "`pop_domain` values within range") |>
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
