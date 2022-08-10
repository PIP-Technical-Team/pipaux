#' pipaux: A package for computating the notorious bar statistic
#'
#' Description bla blab
#'
#' @section pipaux functions:
#' The pipaux functions ...
#'
#' @docType package
#' @name pipaux
#' @import data.table
#' @importFrom pipload pip_create_globals add_gls_to_env
#' @importFrom magrittr "%>%" "%<>%"
#' @importFrom glue glue

# Make sure data.table knows we know we're using it
#' @noRd
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
      "..byv",
      "..grs",
      "..tokeep",
      "..vars",
      "GDP",
      "PCE",
      "agegroup",
      "agegroup_label",
      "country_name",
      "education",
      "gender",
      "grouping_type",
      "indicator_code",
      "inpovcal"
    ),
    package = utils::packageName()
  )
}



NULL
