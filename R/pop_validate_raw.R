#' Validate pop raw data download from wdi
#'
#' @param pop raw pop data, as loaded via `wbstats::wb_data`
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#'
#' @export
pop_validate_raw <- function(pop, detail = getOption("pipaux.detail.output")){

  stopifnot("WB POP raw data is not loaded" = !is.null(pop))

  report <- data_validation_report()

  validate(pop, name = "WB POP raw data validation") |>
    validate_if(is.character(indicator_id),
                description = "`indicator_id` should be character") |>
    validate_cols(in_set(c("SP.POP.TOTL", "SP.RUR.TOTL", "SP.URB.TOTL")),
                  indicator_id, description = "`indicator_id` values within range") |>
    validate_if(is.character(indicator),
                description = "`indicator` should be character") |>
    validate_if(is.character(iso2c),
                description = "`iso2c` should be character") |>
    validate_if(is.character(iso3c),
                description = "`iso3c` should be character") |>
    validate_if(is.character(country),
                description = "`country` should be character") |>
    validate_if(is.numeric(date),
                description = "`date` should be numeric") |>
    validate_if(is.numeric(value),
                description = "`value` should be numeric") |>
    validate_if(is.character(unit),
                description = "`unit` should be character") |>
    validate_if(is.character(obs_status),
                description = "`obs_status` should be character") |>
    validate_if(is.character(footnote),
                description = "`footnote` should be character") |>
    validate_if(is_date(last_updated),
                description = "`last_updated` should be date") |>
    validate_cols(not_na, indicator_id, iso3c, date,
                  description = "no missing values in key variables") |>
    validate_if(is_uniq(indicator_id, iso3c, date),
                description = "no duplicate records in key variables") |>
    add_results(report)

  validation_record <- get_results(report, unnest = FALSE) |>
    setDT()

  if (any(validation_record[["type"]] == "error")){
    get_error_validation(validation_record, detail)
  }

}
