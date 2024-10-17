#' Validate raw ppp data
#'
#' @param ppp raw ppp data, as loaded via `pipfun::load_from_gh`
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#' @keywords internal
#'
#' @export
ppp_validate_raw <- function(ppp, detail = getOption("pipaux.detail.raw")){

  stopifnot("PPP raw data is not loaded" = !is.null(ppp))

  report <- data_validation_report()

  validate(ppp, name = "PPP raw data validation") |>
    validate_if(is.character(CountryName),
                description = "`CountryName` should be character") |>
    validate_if(is.character(code),
                description = "`code` should be character") |>
    validate_if(is.character(CoverageType),
                description = "`CoverageType` should be character") |>
    validate_cols(in_set(c("National", "Rural", "Urban")),
                  CoverageType, description = "`CoverageType` values within range") |>
    validate_if(is.numeric(ppp_2005_v1_v1),
                description = "`ppp_2005_v1_v1` should be numeric") |>
    validate_if(is.numeric(ppp_2011_v1_v1),
                description = "`ppp_2011_v1_v1` should be numeric") |>
    validate_if(is.numeric(ppp_2011_v2_v1),
                description = "`ppp_2011_v2_v1` should be numeric") |>
    validate_if(is.numeric(ppp_2011_v1_v2),
                description = "`ppp_2011_v1_v2` should be numeric") |>
    validate_if(is.numeric(ppp_2011_v2_v2),
                description = "`ppp_2011_v2_v2` should be numeric") |>
    validate_if(is.numeric(ppp_2017_v1_v1),
                description = "`ppp_2017_v1_v1` should be numeric") |>
    validate_if(is.numeric(ppp_2017_v1_v2),
                description = "`ppp_2017_v1_v2` should be numeric") |>
    validate_if(is.numeric(source_ppp_2011),
                description = "`source_ppp_2011` should be numeric") |>
    validate_if(is.numeric(source_ppp_2005),
                description = "`source_ppp_2005` should be numeric") |>
    validate_if(is.numeric(datalevel),
                description = "`datalevel` should be numeric") |>
    validate_cols(in_set(c(0, 1, 2)),
                  datalevel, description = "`datalevel` values within range") |>
    validate_if(is.numeric(ppp_domain),
                description = "`ppp_domain` should be numeric") |>
    validate_cols(in_set(c(1, 2)),
                  ppp_domain, description = "`ppp_domain` values within range") |>
    validate_if(is.numeric(ppp_domain_value),
                description = "`ppp_domain_value` should be numeric") |>
    validate_cols(in_set(c(1, 2)),
                  ppp_domain_value, description = "`ppp_domain_value` values within range") |>
    validate_if(is.numeric(oldicp2005),
                description = "`oldicp2005` should be numeric") |>
    validate_if(is.numeric(oldicp2011),
                description = "`oldicp2011` should be numeric") |>
    validate_if(is.character(Seriesname),
                description = "`Seriesname` should be character") |>
    validate_if(is.character(note_may192020),
                description = "`note_may192020` should be character") |>
    validate_if(is.character(ppp_2017_v1_v2_note),
                description = "`ppp_2017_v1_v2_note` should be character") |>
    validate_cols(not_na, code, CoverageType, datalevel,
                  description = "no missing values in key variables") |>
    validate_if(is_uniq(code, CoverageType, datalevel),
                description = "no duplicate records in key variables") |>
    add_results(report)

  validation_record <- get_results(report, unnest = FALSE) |>
    setDT()

  if (any(validation_record[["type"]] == "error")){
    get_error_validation(validation_record, detail)
  }

}
