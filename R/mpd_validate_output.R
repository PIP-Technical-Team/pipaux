#' Validate raw maddison data
#'
#' @param mpd output mpd data
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#'
#' @export
mpd_validate_raw <- function(mpd, detail = getOption("pipaux.detail.raw")){

  stopifnot("mpd/ maddison output data is not loaded" = !is.null(mpd))

  report <- data_validation_report()

  validate(mpd, name = "mdp output data validation") |>
    validate_if(is.character(country_code),
                description = "`country_code` should be character") |>
    validate_if(is.numeric(year),
                description = "`year` should be numeric") |>
    validate_if(is.numeric(mpd_gdp),
                description = "`mpd_gdp` should be numeric") |>
    validate_cols(not_na, country_code, year,
                  description = "no missing values in key variables") |>
    validate_if(is_uniq(country_code, year),
                description = "no duplicate records in key variables") |>
    add_results(report)

  validation_record <- get_results(report, unnest = FALSE) |>
    setDT()

  if (any(validation_record[["type"]] == "error")){
    get_error_validation(validation_record, detail)
  }

}
