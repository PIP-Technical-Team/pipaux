#' Validate raw wdi data
#'
#' @param wdi raw wdi data, as loaded via `pipfun::load_from_gh`
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#'
#' @export
wdi_validate_raw <- function(wdi, detail = getOption("pipaux.detail.raw")){

  stopifnot("WDI raw data is not loaded" = !is.null(wdi))

  report <- data_validation_report()

  validate(wdi, name = "WDI raw data validation") |>
    validate_if(is.character(country_code),
                description = "`country_code` should be character") |>
    validate_if(is.numeric(year),
                description = "`year` should be numeric") |>
    validate_if(is.numeric(NE.CON.PRVT.PC.KD),
                description = "`NE.CON.PRVT.PC.KD` should be numeric") |>
    validate_if(is.numeric(NY.GDP.PCAP.KD),
                description = "`NY.GDP.PCAP.KD` should be numeric") |>
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
