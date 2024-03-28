#' Validate raw special cases pop data
#'
#' @param spop raw special case pop data, as loaded via `pipfun::load_from_gh`
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#'
#' @export
spop_validate_raw <- function(spop, detail = getOption("pipaux.detail.output")){

  stopifnot("Special POP raw data is not loaded" = !is.null(spop))

  report <- data_validation_report()

  validate(spop, name = "Special POP raw data validation") |>
    validate_if(is.character(country_code),
                description = "`country_code` should be character") |>
    validate_if(is.numeric(year),
                description = "`year` should be numeric") |>
    validate_if(is.numeric(pop_data_level),
                description = "`pop_data_level` should be numeric") |>
    validate_cols(in_set(c(0, 1, 2)),
                  pop_data_level, description = "`pop_data_level` values within range") |>
    validate_if(is.numeric(pop),
                description = "`pop` should be numeric") |>
    validate_cols(not_na, country_code, year, pop_data_level,
                  description = "no missing values in key variables") |>
    validate_if(is_uniq(country_code, year, pop_data_level),
                description = "no duplicate records in key variables") |>
    add_results(report)

  validation_record <- get_results(report, unnest = FALSE) |>
    setDT()

  if (any(validation_record[["type"]] == "error")){
    get_error_validation(validation_record, detail)
  }

}
