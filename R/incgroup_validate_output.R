#' Validate income group output data
#'
#' @param incgroup income group output data
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#'
#' @export
incgroup_validate_output <- function(incgroup, detail = getOption("pipaux.detail.output")){

  stopifnot("Income group output data is not loaded" = !is.null(incgroup))

  report <- data_validation_report()

  validate(incgroup, name = "Income group output data validation") |>
    validate_if(is.character(country_code),
                description = "`country_code` should be character") |>
    validate_if(is.numeric(year_data),
                description = "`year_data` should be numeric") |>
    validate_if(is.character(incgroup_historical),
                description = "`incgroup_historical` should be character") |>
    validate_cols(in_set(c("High income", "Low income", "Lower middle income", "Upper middle income")),
                  incgroup_historical, description = "`incgroup_historical` values within range") |>
    validate_if(is.character(fcv_historical),
                description = "`fcv_historical` should be character") |>
    validate_if(is.character(ssa_subregion_code),
                description = "`ssa_subregion_code` should be character") |>
    validate_cols(not_na, country_code, year_data,
                  description = "no missing values in key variables") |>
    validate_if(is_uniq(country_code, year_data),
                description = "no duplicate records in key variables") |>
    add_results(report)

  validation_record <- get_results(report, unnest = FALSE) |>
    setDT()

  if (any(validation_record[["type"]] == "error")){
    get_error_validation(validation_record, detail)
  }

}
