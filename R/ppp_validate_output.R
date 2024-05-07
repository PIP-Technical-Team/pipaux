#' Validate output ppp data
#'
#' @param ppp output ppp data
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#' @keywords internal
#'
#' @export
ppp_validate_output <- function(ppp, detail = getOption("pipaux.detail.output")){

  stopifnot("PPP output data is not loaded" = !is.null(ppp))

  report <- data_validation_report()

  validate(ppp, name = "PPP output data validation") |>
    validate_if(is.character(country_code),
                description = "`country_code` should be character") |>
    validate_if(is.numeric(ppp_year),
                description = "`ppp_year` should be character") |>
    validate_if(is.character(release_version),
                description = "`release_version` should be character") |>
    validate_if(is.character(adaptation_version),
                description = "`adaptation_version` should be character") |>
    validate_if(is.numeric(ppp),
                description = "`ppp` should be numeric") |>
    validate_if(is.logical(ppp_default),
                description = "`ppp_default` should be numeric") |>
    validate_if(is.logical(ppp_default_by_year),
                description = "`ppp_default_by_year` should be numeric") |>
    validate_if(is.character(ppp_domain),
                description = "`ppp_domain` should be character") |>
    validate_cols(in_set(c("1", "2")),
                  ppp_domain, description = "`ppp_domain` values within range") |>
    validate_if(is.character(ppp_data_level),
                description = "`ppp_data_level` should be character") |>
    validate_cols(in_set(c("national", "rural", "urban")),
                  ppp_data_level, description = "`ppp_data_level` values within range") |>
    validate_cols(not_na, country_code, ppp_year, ppp_data_level,
                  adaptation_version, release_version,
                  description = "no missing values in key variables") |>
    validate_if(is_uniq(country_code, ppp_year,
                        ppp_data_level, adaptation_version, release_version),
                description = "no duplicate records in key variables") |>
    add_results(report)

  validation_record <- get_results(report, unnest = FALSE) |>
    setDT()

  if (any(validation_record[["type"]] == "error")){
    get_error_validation(validation_record, detail)
  }

}
