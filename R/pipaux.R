#' pipaux: A package for computating the notorious bar statistic
#'
#' Description bla blab
#'
#' @section pipaux functions:
#' The pipaux functions ...
#'
#' @keywords internal
#' @docType package
#' @name pipaux
#' @import data.table
#' @importFrom lifecycle deprecated
#' @importFrom pipfun pip_create_globals
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom glue glue
"_PACKAGE"

# Make sure data.table knows we know we're using it
.datatable.aware = TRUE

# Prevent R CMD check from complaining about the use of pipe expressions
# standard data.table variables
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    names = c(
      ".",
      ".I",
      ".N",
      ".SD",
      ".",
      "!!",
      ":=",
      "Series",
      "adaptation_version",
      "admin_region_code",
      "alpha",
      "chain_factor",
      "code",
      "codes",
      "country",
      "country_code",
      "countrycode",
      "coverage",
      "cpi_data_level",
      "cpi_domain",
      "cpi_ppp_id",
      "d1",
      "d2",
      "datalevel",
      "distribution",
      "distribution_type",
      "fy_day",
      "fy_month",
      "gdp_data_level",
      "gdp_domain",
      "i.GDP",
      "i.PCE",
      "i.pop",
      "income_level",
      "iso",
      "lending_type",
      "max_days",
      "maxalt",
      "maxid",
      "maxmast",
      "month_num",
      "mpd_gdp",
      "n",
      "new_var",
      "orig",
      "patterns",
      "pce_data_level",
      "pce_domain",
      "pcn_region_code",
      "pop_data_level",
      "pop_domain",
      "ppp_data_level",
      "ppp_domain",
      "ppp_year",
      "read.csv",
      "ref_year",
      "region",
      "region_code",
      "release_version",
      "rep_var",
      "rep_var_lag",
      "rep_var_lead",
      "reporting_year",
      "shared_prosperity",
      "sna_pce",
      "subject_code",
      "survey_conductor",
      "survey_coverage",
      "survey_title",
      "survey_year",
      "surveyid_year",
      "survname",
      "ver",
      "veralt",
      "vermast",
      "wdi_gdp",
      "wdi_gdp_cy",
      "wdi_gdp_lag",
      "wdi_gdp_lead",
      "wdi_gdp_tmp",
      "wdi_pce",
      "wdi_pce_cy",
      "wdi_pce_lag",
      "wdi_pce_lead",
      "wdi_pce_tmp",
      "welfare_type",
      "weo_gdp_lcu",
      "weo_gdp_lcu_notpc",
      "weo_subject_code",
      "year2",
      "year_range",
      "..keep_vars",
      "..tokeep",
      "..vars",
      "PCE",
      "bck",
      "domain_check",
      "filename",
      "fwd",
      "gd_type",
      "id",
      "inpovcal",
      "module",
      "na.omit",
      "name",
      "pcn_source_file",
      "poverty_line",
      "reporting_level",
      "surv_title",
      "survey_acronym",
      "survey_id",
      "use_groupdata",
      "use_imputed",
      "use_microdata",
      "weo_gdp"
    ),
    package = utils::packageName()
  )
}



NULL
