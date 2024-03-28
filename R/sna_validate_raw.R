#' Validate raw special national accounts (sna) data
#'
#' @param sna raw sna data, as loaded via `pipfun::load_from_gh`
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#'
#' @export
sna_validate_raw <- function(sna, detail = getOption("pipaux.detail.raw")){

  stopifnot("SNA raw data is not loaded" = !is.null(sna))

  report <- data_validation_report()

  validate(sna, name = "SNA raw data validation") |>
    validate_if(is.character(countryname),
                description = "`countryname` should be character") |>
    validate_if(is.character(coverage),
                description = "`coverage` should be character") |>
    validate_cols(in_set(c("National")),
                  coverage, description = "`coverage` values within range") |>
    validate_if(is.character(countrycode),
                description = "`countrycode` should be character") |>
    validate_if(is.numeric(year),
                description = "`year` should be numeric") |>
    validate_if(is.numeric(GDP),
                description = "`GDP` should be numeric") |>
    validate_if(is.logical(PCE),
                description = "`PCE` should be logical") |>
    validate_if(is.character(sourceGDP),
                description = "`sourceGDP` should be character") |>
    validate_if(is.logical(sourcePCE),
                description = "`sourcePCE` should be logical") |>
    validate_cols(not_na, countrycode, year,
                  description = "no missing values in key variables") |>
    validate_if(is_uniq(countrycode, year),
                description = "no duplicate records in key variables") |>
    add_results(report)

  validation_record <- get_results(report, unnest = FALSE) |>
      setDT()

  if (any(validation_record[["type"]] == "error")){
    get_error_validation(validation_record, detail)
  }

}

