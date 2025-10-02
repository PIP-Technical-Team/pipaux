#' PIP PFW
#'
#' Load or update PIP Price Framework data.
#'
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @param action character: Either "load" or "update". Default is "update". If
#' "update" data will be updated on the system. If "load" data is loaded in memory.
#' @param force logical: If TRUE data will be overwritten.
#' @inheritParams pipfun::load_from_gh
#' @export
#' @import data.table
aux_pfw <- function(action  = c("update", "load"),
                    force   = FALSE,
                    owner   = getOption("pipfun.ghowner"),
                    tag     = NULL,
                    detail  = getOption("pipaux.detail.raw")) {
  measure <- "pfw"
  action <- match.arg(action)

  wrk_release <- get_from_auxenv(key = "wrk_release")

  release        <- wrk_release$release
  identity       <- wrk_release$identity
  branch         <- paste0(release, "_", identity)

  if (is.null(tag)) {
    tag <- paste0(release, "_", identity)
  }

  if (action == "update") {
    aux_pfw_update(force   = force,
                   owner   = owner,
                   branch  = branch,
                   tag     = tag,
                   detail  = detail)

  } else {

    dt <- pipload::load_aux_data(measure = measure)

    return(dt)
  }
}
#' Clean PFW
#'
#' Clean PFW data from Datalibweb to meet PIP protocols.
#'
#' @param y dataset with PPP data from `aux_pfw_update()`.
#'
#' @keywords internal
aux_pfw_clean <- function(y) {

  #branch <- match.arg(branch)

  if (!inherits(y, "data.table")) {
    x <- as.data.table(y)
  } else {
    x <- copy(y)
  }

  # get just inpovcal data


  # change variable names
  old_var <-
    c("code",
      "ref_year",
      "survname",
      "comparability",
      "datatype",
      "rep_year"
    )

  new_var <-
    c("country_code",
      "survey_year",
      "survey_acronym",
      "survey_comparability",
      "welfare_type",
      "reporting_year"
    )


  setnames(x,
           old = old_var,
           new = new_var
  )

  # Recode some variables

  x[
    ,
    `:=`(
      # Recode survey coverage
      survey_coverage = fcase(
        survey_coverage == "N", "national",
        survey_coverage == "R", "rural",
        survey_coverage == "U", "urban",
        default = ""
      ),
      # Recode welfare type
      welfare_type = fcase(
        grepl("[Ii]", welfare_type), "income",
        grepl("[Cc]", welfare_type), "consumption",
        default = ""
      ),
      surveyid_year = as.integer(surveyid_year),
      survey_year   = round(survey_year, 2),
      is_alt_welf = FALSE
    )
  ]

  # ---- ADDITION: Handle alternative welfare ----

  if (!all(x$oth_welfare1_type == "")) {

    x_alt <- copy(x[oth_welfare1_type != ""])

    x_alt[
      ,
      welfare_type := fcase(
        grepl("^([Cc])", oth_welfare1_type), "consumption",
        grepl("^([Ii])", oth_welfare1_type), "income",
        default = ""
      )
    ][
      ,
      oth_welfare1_type := NULL
    ][
      ,
      is_alt_welf := TRUE
    ]

    x <- rbindlist(list(x, x_alt), use.names = TRUE, fill = TRUE)

    if (nrow(x) > nrow(unique(x, by = setdiff(names(x), "is_alt_welf")))) {

      cli::cli_alert_info("More than one type of welfare")
    }
  }


  # Load countries and filter

  cl <- pipload::load_aux_data(measure = "country_list")

  x <- x[country_code %in% cl$country_code]

  x <- unique(x) # remove duplicates
  return(x)
}

#' Update PFW
#'
#' @inheritParams aux_pfw
#' @inheritParams pipfun::load_from_gh
#' @keywords internal
aux_pfw_update <- function(force   = FALSE,
                           owner   = getOption("pipfun.ghowner"),
                           branch  = NULL,
                           tag     = NULL,
                           detail  = getOption("pipaux.detail.raw")) {

  measure <- "pfw"
  tag <- branch

  # Read data
  pfw <- pipfun::load_from_gh(measure = measure,
                              owner = owner,
                              branch = branch,
                              ext = "dta")
  # validate pfw raw data
  pfw_validate_raw(pfw = pfw, detail = detail)

  # Clean data
  pfw <- aux_pfw_clean(pfw)

  # validate pfw raw data
  pfw_validate_output(pfw    = pfw,
                      detail = detail)

  # Save dataset
  if (branch == "main") {
    branch <- ""
  }

  # ----- function raw sha ------
  raw_sha_fun <- digest::digest(body(
    paste0("aux_", measure))
  )

  setattr(pfw, "aux_name", "pfw")

  setattr(pfw,
          "raw_sha_fun",
          raw_sha_fun)

  setattr(pfw,
          "aux_key",
          c("country_code", "surveyid_year", "welfare_type"))

  saved <- pip_aux_save(
    x        = pfw,
    pin_name = measure,
    force    = force
  )

  return(invisible(saved))
}

#' Generate a dataset that contains pfw keys
#'
#' @return data.table
#' @export
#'
aux_pfw_key <- function(maindir = getOption("pipaux.working_dir")){

  pfw_temp <- load_aux("pfw",
                       maindir = getOption("pipaux.working_dir"))

  pfw_key_options <- pfw_temp[, .(country_code,
                                  survey_year,
                                  survey_acronym,
                                  cpi_domain_var)]


  cpi_temp <- load_aux("cpi",
                       maindir = getOption("pipaux.working_dir"))

  cpi_temp <- cpi_temp[, cpi_domain_var :=
                         fifelse(reporting_level == "urban" &
                                   cpi_domain_value == 1, "urban", "")]

  cpi_temp <- cpi_temp[, .(country_code, survey_year, survey_acronym,
                           cpi_domain_var, reporting_level)]

  pfw_key <- cpi_temp[pfw_key_options, on = .(country_code, survey_year,
                                              survey_acronym, cpi_domain_var)]

  any(duplicated(pfw_key, by = c("country_code", "survey_year", "survey_acronym", "cpi_domain_var")))

  return(pfw_key)
}

#' Validate raw pfw data
#'
#' @param pfw raw pfw data, as loaded via `pipfun::load_from_gh`
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#' @keywords internal
#'
#' @export
pfw_validate_raw <- function(pfw, detail = getOption("pipaux.detail.raw")){

  stopifnot("PFW raw data is not loaded" = !is.null(pfw))

  report <- data_validation_report()

  validate(pfw, name = "PFW raw data validation") |>
    validate_if(is.character(region),
                description = "`region` should be character") |>
    validate_cols(in_set(c("Sub-Saharan Africa", "Europe & Central Asia", "Middle East, North Africa, Afghanistan & Pakistan",
                           "Middle East, North Africa, Afghanistan & Pakistan", "Latin America & Caribbean", "East Asia & Pacific", "South Asia", "North America")),
                  region, description = "`region` values within range") |>
    validate_if(is.character(code),
                description = "`code` should be character") |>
    validate_if(is.character(reg_pcn),
                description = "`reg_pcn` should be character") |>
    validate_cols(in_set(c("EAP", "ECA", "LAC", "MNA", "OHI", "SAS", "SSA")),
                  reg_pcn, description = "`reg_pcn` values within range") |>
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
    validate_if(is.character(survname),
                description = "`survname` should be character") |>
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
    # validate_cols(in_set(c("national", "partial", "rural", "urban")),
    #               survey_coverage,
    #               description = "`survey_coverage` values within range") |>
    validate_cols(in_set(c("N", "U", "R")),
                  survey_coverage, description = "`survey_coverage` values within range") |>
    validate_if(is.character(datatype),
                description = "`datatype` should be character") |>
    validate_cols(in_set(c("C", "I", "c", "i")),
                  datatype, description = "`datatype` values within range") |>
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
    validate_if(is.numeric(rep_year),
                description = "`rep_year` should be numeric") |>
    validate_if(is.numeric(comparability),
                description = "`comparability` should be numeric") |>
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
    validate_if(is.numeric(ref_year),
                description = "`ref_year` should be numeric") |>
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
    validate_cols(not_na, code, year, survname,
                  description = "no missing values in key variables") |>
    validate_if(is_uniq(code, year, survname),
                description = "no duplicate records in key variables") |>
    add_results(report)

  validation_record <- get_results(report, unnest = FALSE) |>
    setDT()

  if (any(validation_record[["type"]] == "error")){
    get_error_validation(validation_record, detail)
  }

}

#' Validate clean pfw data
#'
#' @param pfw clean pfw data, output via `aux_pfw_clean`
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#' @keywords internal
#'
#' @export
pfw_validate_output <- function(pfw, detail = getOption("pipaux.detail.output")){

  stopifnot("PFW clean data is not loaded" = !is.null(pfw))

  report <- data_validation_report()

  validate(pfw, name = "PFW output data validation") |>
    validate_cols(in_set(c( "SSF", "ECS", "MEA", "LCN", "EAS", "SAS", "NAC")),
                  region_code, description = "`wb_region_code` values within range") |>
    validate_if(is.character(country_code),
                description = "`country_code` should be character") |>
    validate_cols(in_set(c("EAP", "ECA", "LAC", "MNA", "OHI", "SAS", "SSA")),
                  reg_pcn, description = "`reg_pcn` values within range") |>
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
    # validate_cols(in_set(c("national", "rural", "urban")),
    #               survey_coverage, description = "`survey_coverage` values within range") |>
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

# New function to add reporting level var to pfw

pfw_report_lvl <- function(cpfw) {


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # computations   ---------
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    dcols <- c(
      "cpi_domain",
      "ppp_domain",
      "gdp_domain",
      "pce_domain",
      "pop_domain"
    )


    cpfw <-
      cpfw[
        # filter inpovcal data
        inpovcal == 1
      ][,
        # Find MAX domain per obs
        reporting_level := apply(.SD, MARGIN = 1,
                                 function(x) {
                                   y <- max(x)
                                   as.character(y)
                                 }),
        .SDcols = dcols
      ]


    n_cpfw_wt <- length(unique(cpfw$welfare_type))


    if(nrow(cpfw)==0){


      rlang::abort(message = "PFW does not contains info for country, surveyid year, and survey_acronym",
                   class = c("piperr","info_pfw"),
                   use_cli_format = TRUE)


    }else if(nrow(cpfw) > 1 & n_cpfw_wt ==1){


      rlang::abort(message = "PFW is not unique for country, surveyid year, and survey_acronym",
                   class = c("piperr", "no_unq_pfw"),
                   use_cli_format = TRUE)


    }else if(nrow(cpfw)>1){


      rlang::inform(message = "More than one value for country/year PFW",
                    class = c("pipinf", "othr_wlf_inf"),
                    use_cli_format = TRUE)
    }

    # Return   ---------
    return(cpfw)

}







