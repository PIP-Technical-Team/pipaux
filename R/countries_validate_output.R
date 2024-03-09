#' Validate output countries data
#'
#' @param countries output countries data
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#'
#' @export
countries_validate_output <- function(countries, detail = getOption("pipaux.detail.output")){

  stopifnot("Countries output data is not loaded" = !is.null(countries))

  report <- data_validation_report()

  validate(countries, name = "countries output data validation") |>
    validate_if(is.character(country_code),
                description = "`country_code` should be character") |>
    validate_if(is.character(country_name),
                description = "`country_name` should be character") |>
    validate_if(is.character(africa_split),
                description = "`africa_split` should be character") |>
    validate_cols(in_set(c("Eastern and Southern Africa", "Western and Central Africa", NA)),
                  africa_split, description = "`africa_split` values within range") |>
    validate_if(is.character(africa_split_code),
                description = "`africa_split_code` should be character") |>
    validate_cols(in_set(c("AFE", "AFW", NA)),
                  africa_split_code, description = "`africa_split_code` values within range") |>
    validate_if(is.character(region),
                description = "`region` should be character") |>
    validate_if(is.character(region_code),
                description = "`region_code` should be character") |>
    validate_cols(in_set(c("EAP", "ECA", "LAC", "MNA", "OHI", "SAS", "SSA")),
                  region_code, description = "`region_code` values within range") |>
    validate_if(is.character(world),
                description = "`world` should be character") |>
    validate_cols(in_set(c("World")),
                  world, description = "`world` values within range") |>
    validate_if(is.character(world_code),
                description = "`world_code` should be character") |>
    validate_cols(in_set(c("WLD")),
                  world_code, description = "`world_code` values within range") |>
    validate_cols(not_na, country_code,
                  description = "no missing values in key variables") |>
    validate_if(is_uniq(country_code),
                description = "no duplicate records in key variables") |>
    add_results(report)

  validation_record <- get_results(report, unnest = FALSE) |>
    setDT()

  if (any(validation_record[["type"]] == "error")){
    get_error_validation(validation_record, detail)
  }

}

