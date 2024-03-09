#' Validate output gdp data
#'
#' @param gdp output gdp data
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#'
#' @export
gdp_validate_output <- function(gdp, detail = getOption("pipaux.detail.output")){

  stopifnot("GDP output data is not loaded" = !is.null(gdp))

  report <- data_validation_report()

  validate(gdp, name = "GDP output data validation") |>
    validate_if(is.character(country_code),
                description = "`country_code` should be character") |>
    validate_if(is.numeric(year),
                description = "`year` should be numeric") |>
    validate_if(is.character(gdp_data_level),
                description = "`gdp_data_level` should be character") |>
    validate_cols(in_set(c("national", "rural", "urban")),
                  gdp_data_level, description = "`gdp_data_level` values within range") |>
    validate_if(is.numeric(gdp),
                description = "`gdp` should be numeric") |>
    validate_if(is.character(gdp_domain),
                description = "`gdp_domain` should be character") |>
    validate_cols(in_set(c("national", "urban/rural")),
                  gdp_domain, description = "`gdp_domain` values within range") |>
    validate_cols(not_na, country_code, year, gdp_data_level,
                  description = "no missing values in key variables") |>
    validate_if(is_uniq(country_code, year, gdp_data_level),
                description = "no duplicate records in key variables") |>
    add_results(report)

  validation_record <- get_results(report, unnest = FALSE) |>
    setDT()

  if (any(validation_record[["type"]] == "error")){
    get_error_validation(validation_record, detail)
  }

}
