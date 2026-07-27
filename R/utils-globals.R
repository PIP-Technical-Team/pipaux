#' Global variables for NSE
#'
#' This file declares global variables used in non-standard evaluation (NSE)
#' contexts throughout the package to avoid R CMD check NOTEs.
#'
#' @keywords internal
#' @noRd

# Declare all global variables used in data.table/dplyr NSE contexts
utils::globalVariables(c(
  # Common variables
  "year_data", "fcv", ".data", ".joyn", "change_type", "df",
  
  # CPI related
  "cpi_year", "countryname", "cpi_domain_value", "cpi2017_unadj", "cpi2011_unadj",
  "cpi2011", "cpi2017", "cpi2005", "cpi_id", "comparability", "cur_adj",
  "comparable", "change_cpi2017", "change_icp2017", "change_cpi2011",
  "change_icp2011", "ccf", "change_cpi2011", "cpi_domain_var", "cpi_replication",
  
  # GDP/economic variables
  "..new_nms", "new_gdp", "sna_gdp", "last_year", "last_gdp", "cum_growth",
  "gdppc_growth", "projected_GDP", "GDP", "sourceGDP", "sourcePCE",
  "weo_gdp_ppp2017", "NE.CON.PRVT.PC.KD", "NY.GDP.PCAP.KD",
  
  # Income groups and regions
  "incgroup", "income_group_code", "income_group", "ssa_subregion_code",
  "africa_split", "africa_split_code", "regionpcn", "regionpcn_code",
  "world", "world_code", "reg_pcn",
  
  # Log variables
  "logmeta", "step", "event",
  
  # NPL variables
  "nat_headcount", "vsi_pov_nahc_nc", "vsi_pov_nahc", "footnote",
  
  # PFW variables
  "oth_welfare1_type", "is_alt_welf", "ctryname", "timewp", "fieldwork",
  "link", "altname", "survey_time", "wbint_link", "wbext_link", "alt_link",
  "pip_meta", "surv_producer", "datatype", "use_bin", "rep_year", "comp_note",
  "preferable", "display_cp", "fieldwork_range", "newref", "ref_year_des",
  "wf_baseprice", "wf_baseprice_note", "wf_baseprice_des", "wf_spatial_des",
  "wf_spatial_var", "wf_currency_des", "ppp_replication", "ppp_domain_var",
  "wf_add_temp_des", "wf_add_temp_var", "wf_add_spatial_des",
  "wf_add_spatial_var", "tosplit", "tosplit_var", "oth_welfare1_var",
  "pfw_id", "survey_comparability",
  
  # PL variables
  "is_default", "is_visible",
  
  # Population variables
  "indicator_id", "indicator", "iso2c", "iso3c", "value", "unit",
  "obs_status", "last_updated", "pop",
  
  # PPP variables
  "ppp_default", "ppp_default_by_year", "CountryName", "CoverageType",
  "ppp_2005_v1_v1", "ppp_2011_v1_v1", "ppp_2011_v2_v1", "ppp_2011_v1_v2",
  "ppp_2011_v2_v2", "ppp_2017_v1_v1", "ppp_2017_v1_v2", "source_ppp_2011",
  "source_ppp_2005", "ppp_domain_value", "oldicp2005", "oldicp2011",
  "Seriesname", "note_may192020",
  
  # GDM variables
  "survey_mean_lcu", "pcn_survey_id", "Region", "countryName", "Coverage",
  "CountryCode", "SurveyTime", "CPI_Time", "DataType", "SurveyMean_LCU",
  "currency", "SurveyID", "SurveyMean_PPP", "DistributionFileName", "Comment",
  
  # Metadata variables
  "status", "reg", "svy_id", "title", "data_access", "year_start", "year_end",
  "authoring_entity_name", "authoring_entity_affiliation", "contact_email",
  "contact_uri", "abstract", "collection_dates_cycle", "collection_dates_start",
  "collection_dates_end", "sampling_procedure", "collection_mode",
  "coll_situation", "weight", "cleaning_operations", "coverage_notes",
  
  # Validation variables
  "type", "table_name", "description",
  
  # SNA variables
  "Code", "LongName", "SpecialNotes", "Month", "Day", "wrk_release",
  
  # WEO variables
  "WEO Subject Code", "WEO Country Code", "ISO", "Country", "Subject Descriptor",
  "Subject Notes", "Units", "Scale", "Estimates Start After",
  
  # Other variables
  "Year"
))
