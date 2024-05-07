#' Validate raw gdm data
#'
#' @param gdm raw gdm data, as loaded via `pipfun::load_from_gh`
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#' @keywords internal
#'
#' @export
gdm_validate_raw <- function(gdm, detail = getOption("pipaux.detail.raw")){

  stopifnot("GDM raw data is not loaded" = !is.null(gdm))

  report <- data_validation_report()

  validate(gdm, name = "GDM raw data validation") |>
    validate_if(is.character(Region),
                description = "`Region` should be character") |>
    validate_cols(in_set(c("SSA", "ECA", "OHI", "LAC", "SAS", "EAP", "MNA")),
                  Region, description = "`Region` values within range") |>
    validate_if(is.character(countryName),
                description = "`countryName` should be character") |>
    validate_if(is.character(Coverage),
                description = "`Coverage` should be character") |>
    validate_cols(in_set(c("National", "Urban", "Aggregated", "Rural", "rural", "urban")),
                  Coverage, description = "`Coverage` values within range") |>
    validate_if(is.character(CountryCode),
                description = "`CountryCode` should be character") |>
    validate_if(is.numeric(SurveyTime),
                description = "`SurveyTime` should be numeric") |>
    validate_if(is.numeric(CPI_Time),
                description = "`CPI_Time` should be numeric") |>
    validate_if(is.character(DataType),
                description = "`DataType` should be character") |>
    validate_cols(in_set(c("x", "X", "y", "Y")),
                  DataType, description = "`DataType` values within range") |>
    validate_if(is.numeric(SurveyMean_LCU),
                description = "`SurveyMean_LCU` should be numeric") |>
    validate_if(is.numeric(currency),
                description = "`currency` should be numeric") |>
    validate_if(is.character(source),
                description = "`source` should be character") |>
    validate_if(is.character(SurveyID),
                description = "`SurveyID` should be character") |>
    validate_if(is.numeric(SurveyMean_PPP),
                description = "`SurveyMean_PPP` should be numeric") |>
    validate_if(is.character(DistributionFileName),
                description = "`DistributionFileName` should be character") |>
    validate_cols(is.logical, Comment, description = "Comment should be logical") |>
    validate_cols(not_na, CountryCode, Coverage, SurveyTime, DataType,
                  description = "no missing values in key variables") |>
    validate_if(is_uniq(CountryCode, Coverage, SurveyTime, DataType),
                description = "no duplicate records in key variables") |>
    add_results(report)

  validation_record <- get_results(report, unnest = FALSE) |>
      setDT()

  if (any(validation_record[["type"]] == "error")){
    get_error_validation(validation_record, detail)
  }

}

