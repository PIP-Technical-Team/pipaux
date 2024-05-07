#' Validate npl output data
#'
#' @param npl output data
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#' @keywords internal
#'
#' @export
npl_validate_output <- function(npl, detail = getOption("pipaux.detail.output")){

  stopifnot("NPL output data is not loaded" = !is.null(npl))

  report <- data_validation_report()

  validate(npl, name = "NPL output data validation") |>
    validate_if(is.character(country_code),
                description = "`country_code` should be character") |>
    validate_if(is.numeric(reporting_year),
                description = "`reporting_year` should be numeric") |>
    validate_if(is.numeric(nat_headcount),
                description = "`nat_headcount` should be numeric") |>
    validate_if(is.numeric(comparability),
                description = "`comparability` should be numeric") |>
    validate_if(is.character(footnote),
                description = "`footnote` should be character") |>
    validate_cols(not_na, country_code, reporting_year,
                  description = "no missing values in key variables") |>
    validate_if(is_uniq(country_code, reporting_year),
                description = "no duplicate records in key variables") |>
    add_results(report)

  validation_record <- get_results(report, unnest = FALSE) |>
    setDT()

  if (any(validation_record[["type"]] == "error")){
    get_error_validation(validation_record, detail)
  }

}
