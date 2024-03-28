#' Validate raw main pop data
#'
#' @param pop_main raw pop main data, as loaded via `pipfun::load_from_gh`
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#'
#' @export
popmain_validate_raw <- function(pop_main, detail = getOption("pipaux.detail.raw")){

  stopifnot("POP main raw data is not loaded" = !is.null(pop_main))

  report <- data_validation_report()

  validate(pop_main, name = "POP main raw data validation") |>
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
