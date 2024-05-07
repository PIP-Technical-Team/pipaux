#' Validate raw cpi data
#'
#' @param cpi raw cpi data, as loaded via `pipfun::load_from_gh`
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#' @keywords internal
#'
#' @export
cpi_validate_raw <- function(cpi, detail = getOption("pipaux.detail.raw")){

  stopifnot("CPI raw data is not loaded" = !is.null(cpi))

  report <- data_validation_report()

  validate(cpi, name = "CPI raw data validation") |>
    validate_if(is.character(region),
                description = "`region` should be character") |>
    validate_cols(in_set(c("EAP", "ECA", "LAC", "MNA", "NAC", "SAR", "SSA")),
                  region, description = "`region` values within range") |>
    validate_if(is.character(code),
                description = "`code` should be character") |>
    validate_if(is.character(countryname),
                description = "`countryname` should be character") |>
    validate_if(is.numeric(year),
                description = "`year` should be numeric") |>
    validate_if(is.character(survname),
                description = "`survname` should be character") |>
    validate_if(is.numeric(ref_year),
                description = "`ref_year` should be numeric") |>
    validate_if(is.character(cpi_domain),
                description = "`cpi_domain` should be character") |>
    validate_cols(in_set(c("National", "Urban/Rural")),
                  cpi_domain, description = "`cpi_domain` values within range") |>
    validate_if(is.numeric(cpi_domain_value),
                description = "`cpi_domain_value` should be numeric") |>
    validate_if(is.numeric(cpi2017_unadj),
                description = "`cpi2017_unadj` should be numeric") |>
    validate_if(is.numeric(cpi2011_unadj),
                description = "`cpi2011_unadj` should be numeric") |>
    validate_if(is.numeric(cpi2011),
                description = "`cpi201`1 should be numeric") |>
    validate_if(is.numeric(cpi2017),
                description = "`cpi2017` should be numeric") |>
    validate_if(is.character(version),
                description = "`version` should be character") |>
    validate_if(is.numeric(comparability),
                description = "`comparability` should be numeric") |>
    validate_if(is.numeric(cur_adj),
                description = "`cur_adj` should be numeric") |>
    validate_if(is.character(survey_coverage),
                description = "`survey_coverage` should be character") |>
    validate_cols(in_set(c("N", "R", "U", NA)),
                  survey_coverage, description = "`survey_coverage` values within range") |>
    validate_if(is.numeric(cpi2011_SM22),
                description = "`cpi2011_SM22` should be numeric") |>
    validate_if(is.numeric(comparable),
                description = "`comparable` should be numeric") |>
    validate_if(is.numeric(cpi2017_SM22),
                description = "`cpi2017_SM22` should be numeric") |>
    validate_cols(is.logical, cpi2005,
                  description = "`cpi2005` should be logical") |>
    validate_if(is.numeric(cpi_data_level),
                description = "`cpi_data_level` should be numeric") |>
    validate_cols(in_set(c(0, 1, 2)),
                  cpi_data_level, description = "`cpi_data_level` values within range") |>
    validate_if(is.numeric(ref_year_AM23),
                description = "`ref_year_AM23` should be numeric") |>
    validate_if(is.numeric(cpi2011_AM23),
                description = "`cpi2011_AM23` should be numeric") |>
    validate_if(is.numeric(cpi2017_AM23),
                description = "`cpi2011_AM23` should be numeric") |>
    validate_if(is.numeric(change_cpi2017),
                description = "`change_cpi2017` should be numeric") |>
    validate_if(is.numeric(change_icp2017),
                description = "`change_icp2017` should be numeric") |>
    validate_if(is.numeric(change_cpi2011),
                description = "`change_cpi2011` should be numeric") |>
    validate_if(is.numeric(change_icp2011),
                description = "`change_icp2011` should be numeric") |>
    validate_if(is.character(cpi_id),
                description = "`cpi_id` should be character") |>
    validate_cols(not_na, code, year, survname, cpi_data_level,
                  description = "no missing values in key variables") |>
    validate_if(is_uniq(code, year,  survname, cpi_data_level),
                description = "no duplicate records in key variables") |>
    add_results(report)

  validation_record <- get_results(report, unnest = FALSE) |>
      setDT()

  if (any(validation_record[["type"]] == "error")){
    get_error_validation(validation_record, detail)
  }

}
