#' Validate clean pfw data
#'
#' @param pfw clean pfw data, output via `pipfun::pip_pfw_clean`
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#'
#' @export
pfw_validate_output <- function(pfw, detail = getOption("pipaux.detail.output")){

  stopifnot("PFW clean data is not loaded" = !is.null(pfw))

  report <- data_validation_report()

  validate(pfw, name = "PFW output data validation") |>
    validate_if(is.character(wb_region_code),
                description = "`wb_region_code` should be character") |>
    validate_cols(in_set(c("EAP", "ECA", "LAC", "MNA", "NAC", "SAR", "SSA")),
                  wb_region_code, description = "`wb_region_code` values within range") |>
    validate_if(is.character(country_code),
                description = "`country_code` should be character") |>
    validate_if(is.character(pcn_region_code),
                description = "`pcn_region_code` should be character") |>
    validate_cols(in_set(c("EAP", "ECA", "LAC", "MNA", "OHI", "SAS", "SSA")),
                  pcn_region_code, description = "`pcn_region_code` values within range") |>
    validate_if(is.character(ctryname),
                description = "`ctryname` should be character") |>
    validate_if(is.numeric(year),
                description = "`year` should be numeric") |>
    validate_if(is.numeric(surveyid_year),
                description = "`surveyid_year` should be numeric") |>
    validate_if(is.numeric(timewp),
                description = "`timewp` should be numeric") |>
    validate_if(is.numeric(fieldwork),
                description = "`fieldwork` should be numeric") |>
    validate_if(is.character(survey_acronym),
                description = "`survey_acronym` should be character") |>
    validate_if(is.character(link),
                description = "`link` should be character") |>
    validate_if(is.character(altname),
                description = "`altname` should be character") |>
    validate_if(is.character(survey_time),
                description = "`survey_time` should be character") |>
    validate_if(is.numeric(wbint_link),
                description = "`wbint_link` should be numeric") |>
    validate_if(is.numeric(wbext_link),
                description = "`wbext_link` should be numeric") |>
    validate_if(is.numeric(alt_link),
                description = "`alt_link` should be numeric") |>
    validate_if(is.numeric(pip_meta),
                description = "`pip_meta` should be numeric") |>
    validate_if(is.character(surv_title),
                description = "`surv_title` should be character") |>
    validate_if(is.character(surv_producer),
                description = "`surv_producer` should be character") |>
    validate_if(is.character(survey_coverage),
                description = "`survey_coverage` should be character") |>
    validate_cols(in_set(c("national", "rural", "urban")),
                  survey_coverage, description = "`survey_coverage` values within range") |>
    validate_if(is.character(welfare_type),
                description = "`welfare_type` should be character") |>
    validate_cols(in_set(c("consumption", "income")),
                  welfare_type, description = "`welfare_type` values within range") |>
    validate_if(is.numeric(use_imputed),
                description = "`use_imputed` should be numeric") |>
    validate_cols(in_set(c(0, 1)),
                  use_imputed, description = "`use_imputed` values within range") |>
    validate_if(is.numeric(use_microdata),
                description = "`use_microdata` should be numeric") |>
    validate_cols(in_set(c(0, 1)),
                  use_microdata, description = "`use_microdata` values within range") |>
    validate_if(is.numeric(use_bin),
                description = "`use_bin` should be numeric") |>
    validate_cols(in_set(c(0, 1)),
                  use_bin, description = "`use_bin` values within range") |>
    validate_if(is.numeric(use_groupdata),
                description = "`use_groupdata` should be numeric") |>
    validate_cols(in_set(c(0, 1)),
                  use_groupdata, description = "`use_groupdata` values within range") |>
    validate_if(is.numeric(reporting_year),
                description = "`reporting_year` should be numeric") |>
    validate_if(is.numeric(survey_comparability),
                description = "`survey_comparability` should be numeric") |>
    validate_if(is.character(comp_note),
                description = "`comp_note` should be character") |>
    validate_if(is.character(preferable),
                description = "`preferable` should be character") |>
    validate_if(is.numeric(display_cp),
                description = "`display_cp` should be numeric") |>
    validate_cols(in_set(c(0, 1)),
                  display_cp, description = "`display_cp` values within range") |>
    validate_if(is.character(fieldwork_range),
                description = "`fieldwork_range` should be character") |>
    validate_if(is.numeric(survey_year),
                description = "`survey_year` should be numeric") |>
    validate_if(is.character(newref),
                description = "`newref` should be character") |>
    validate_if(is.numeric(ref_year_des),
                description = "`ref_year_des` should be numeric") |>
    validate_if(is.character(wf_baseprice),
                description = "`wf_baseprice` should be character") |>
    validate_if(is.character(wf_baseprice_note),
                description = "`wf_baseprice_note` should be character") |>
    validate_if(is.numeric(wf_baseprice_des),
                description = "`wf_baseprice_des` should be numeric") |>
    validate_cols(in_set(c(-9, -8, -7)), wf_baseprice_des,
                  description = "`wf_baseprice_des` values within range") |>
    validate_if(is.numeric(wf_spatial_des),
                description = "`wf_spatial_des` should be numeric") |>
    validate_if(is.character(wf_spatial_var),
                description = "`wf_spatial_var` should be character") |>
    validate_if(is.numeric(cpi_replication),
                description = "`cpi_replication` should be numeric") |>
    validate_cols(in_set(c(-9, 1)),
                  cpi_replication, description = "`cpi_replication` values within range") |>
    validate_if(is.numeric(cpi_domain),
                description = "`cpi_domain` should be numeric") |>
    validate_cols(in_set(c(1, 2)),
                  cpi_domain, description = "`cpi_domain` values within range") |>
    validate_if(is.character(cpi_domain_var),
                description = "`cpi_domain_var` should be character") |>
    validate_if(is.numeric(wf_currency_des),
                description = "`wf_currency_des` should be numeric") |>
    validate_cols(in_set(c(0, 2)),
                  wf_currency_des, description = "`wf_currency_des` values within range") |>
    validate_if(is.numeric(ppp_replication),
                description = "`ppp_replication` should be numeric") |>
    validate_cols(in_set(c(-9, 1)),
                  ppp_replication, description = "`ppp_replication` values within range") |>
    validate_if(is.numeric(ppp_domain),
                description = "`ppp_domain` should be numeric") |>
    validate_cols(in_set(c(1, 2)),
                  ppp_domain, description = "`ppp_domain` values within range") |>
    validate_if(is.character(ppp_domain_var),
                description = "`ppp_domain_var` should be character") |>
    validate_if(is.numeric(wf_add_temp_des),
                description = "`wf_add_temp_des` should be numeric") |>
    validate_cols(in_set(c(-9, 0)),
                  wf_add_temp_des, description = "`wf_add_temp_des` values within range") |>
    validate_if(is.numeric(wf_add_temp_var),
                description = "`wf_add_temp_var` should be numeric") |>
    validate_if(is.numeric(wf_add_spatial_des),
                description = "`wf_add_spatial_des` should be numeric") |>
    validate_cols(in_set(c(-9, 0, 1)), wf_add_spatial_des,
                  description = "`wf_add_spatial_des` values within range") |>
    validate_if(is.numeric(wf_add_spatial_var),
                description = "`wf_add_spatial_var` should be numeric") |>
    validate_if(is.numeric(tosplit),
                description = "`tosplit` should be numeric") |>
    validate_cols(in_set(c(NA, 1)), tosplit,
                  description = "`tosplit` values within range") |>
    validate_if(is.character(tosplit_var),
                description = "`tosplit_var` should be character") |>
    validate_if(is.numeric(inpovcal),
                description = "`inpovcal` should be numeric") |>
    validate_cols(in_set(c(1)), inpovcal,
                  description = "`inpovcal` values within range") |>
    validate_if(is.character(oth_welfare1_type),
                description = "`oth_welfare1_type` should be character") |>
    validate_if(is.character(oth_welfare1_var),
                description = "`oth_welfare1_var` should be character") |>
    validate_if(is.numeric(gdp_domain),
                description = "`gdp_domain` should be numeric") |>
    validate_cols(in_set(c(1, 2)), gdp_domain,
                  description = "`gdp_domain` values within range") |>
    validate_if(is.numeric(pce_domain),
                description = "`pce_domain` should be numeric") |>
    validate_cols(in_set(c(1, 2)), pce_domain,
                  description = "`pce_domain` values within range") |>
    validate_if(is.numeric(pop_domain),
                description = "`pop_domain` should be numeric") |>
    validate_cols(in_set(c(1, 2)), pop_domain,
                  description = "`pop_domain` values within range") |>
    validate_if(is.character(pfw_id),
                description = "`pfw_id` should be character") |>
    validate_cols(not_na, country_code, year, welfare_type,
                  description = "no missing values in key variables") |>
    validate_if(is_uniq(country_code, year, welfare_type),
                description = "no duplicate records in key variables") |>
    add_results(report)

  validation_record <- get_results(report, unnest = FALSE) |>
    setDT()

  if (any(validation_record[["type"]] == "error")){
    get_error_validation(validation_record, detail)
  }

}
