#' PIP CPI
#'
#' Load or update PIP CPI data.
#'
#' @param action character: Either "load" or "update". Default is "update". If
#'   "update" data will be updated on the system. If "load" data is loaded in
#'   memory.
#' @param maindir character: Main directory of project.
#' @param force logical: If TRUE data will be overwritten.
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @inheritParams aux_censoring
#' @inheritParams pipfun::load_from_gh
#'
#' @export
#' @import data.table
aux_cpi <- function(action = c("update", "load"),
                    maindir = gls$PIP_DATA_DIR,
                    force   = FALSE,
                    owner   = getOption("pipfun.ghowner"),
                    tag     = NULL,
                    detail = getOption("pipaux.detail.raw")) {

  pipfun::get_wrk_release(verbose = FALSE)

  release        <- wrk_release$release
  identity       <- wrk_release$identity
  branch         <- paste0(release, "_", identity)

  if (is.null(tag)) {
    tag <- paste0(release, "_", identity)
  }

  #   ____________________________________________________________________________
  #   on.exit                                                                 ####
  on.exit({

  })

  #   ____________________________________________________________________________
  #   Defenses                                                                ####
  measure <- "cpi"
  action <- match.arg(action)

  stopifnot( exprs = {

  }
  )

  #   ____________________________________________________________________________
  #   Early returns                                                           ####
  if (FALSE) {
    return()
  }

  #   ____________________________________________________________________________
  #   Computations                                                            ####
  if (action == "update") {
    aux_cpi_update(maindir = maindir,
                   force   = force,
                   owner   = owner,
                   branch  = branch,
                   tag     = tag,
                   detail  = detail)
  }
  else {
    dt <- load_aux(
      maindir = maindir,
      measure = measure,
      branch  = branch
    )
    return(dt)
  }


}

#' Clean CPI data
#'
#' Clean CPI data from Datalibweb to meet PIP protocols.
#'
#' @param y dataset with CPI data from `aux_cpi_update()`.
#' @param cpivar character: CPI variable to be used as default. Currently it is
#' "cpi2011".
#' @inheritParams aux_cpi_update
#'
#' @keywords internal
aux_cpi_clean <- function(y,
                          cpivar = getOption("pipaux.cpivar"),
                          maindir = gls$PIP_DATA_DIR,
                          branch  = paste0(wrk_release$release, "_", wrk_release$identity)) {

  x <- data.table::as.data.table(y)

  # vars to keep
  keep_vars <- c(
    "country_code", "cpi_year", "survey_year",
    "cpi", "ccf", "survey_acronym", "change_cpi2011",
    grep("^cpi", names(x), value = TRUE)
  )

  # modifications to the database
  x[
    ,
    c("cur_adj", "ccf")
    := {
      cur_adj <- ifelse(is.na(cur_adj), 1, cur_adj)
      ccf <- 1 / cur_adj

      list(cur_adj, ccf)
    }
  ][
    ,
    `:=`(
      country_code   = code,
      cpi_year       = as.integer(year),
      survey_year    = round(ref_year, 2),
      cpi            = get(cpivar),
      survey_acronym = survname,
      cpi_domain     = as.character(cpi_domain),
      cpi_data_level = as.character(cpi_data_level)
    )
  ][
    ,
    # This part should not exist if the raw data
    # had been created properly
    cpi_data_level := fcase(
      tolower(cpi_domain) %chin% c("urban/rural", "2") & cpi_data_level == "0", "rural",
      tolower(cpi_domain) %chin% c("urban/rural", "2") & cpi_data_level == "1", "urban",
      tolower(cpi_domain) %chin% c("national", "1") & cpi_data_level %chin% c("2", "", NA_character_), "national",
      default = ""
    )
  ]
  # keep final vars
  x <- x[, ..keep_vars ]

  x <- unique(x) # remove duplicates

  # Remove any non-WDI countries
  cl <- load_aux(maindir = maindir,
                 measure = "country_list",
                 branch = branch)

  x <- x[country_code %in% cl$country_code]


  return(x)
}

#' Update CPI
#'
#' @inheritParams aux_cpi
#' @keywords internal
aux_cpi_update <- function(maindir = gls$PIP_DATA_DIR,
                           force   = FALSE,
                           owner   = getOption("pipfun.ghowner"),
                           branch = paste0(wrk_release$release, "_", wrk_release$identity),
                           tag,
                           detail  = getOption("pipaux.detail.raw")) {

  #   ____________________________________________________________________________
  #   Set up                                                                  ####

  measure <- "cpi"
  tag <- branch


  #   ____________________________________________________________________________
  #   load raw data                                                           ####

  cpi <- pipfun::load_from_gh(
    measure = measure,
    owner  = owner,
    branch = branch,
    tag    = tag,
    ext    = "csv"
  )


  # validate cpi raw data
  cpi_validate_raw(cpi, detail = detail)

  #   ____________________________________________________________________________
  #   Cleaning                                                                ####

  # Clean data
  cpi <- aux_cpi_clean(cpi,
                       maindir = maindir,
                       branch = branch)

  # drop cpi_domain
  cpi <- cpi[, -c("cpi_domain")]

  # changae cpi_year and cpi_data_level to year and reporting_level
  cpi <- cpi |> setnames(c("cpi_year", "cpi_data_level"),
                         c("year", "reporting_level"),
                         skip_absent=TRUE)

  # ----- function raw sha ------ ####
  raw_sha_fun <- digest::digest(body(
    paste0("aux_", measure))
  )

  # Setting attributes ####

  setattr(cpi,
          "aux_name",
          "cpi")

  setattr(cpi,
          "aux_key",
          c("country_code", "year",
            "reporting_level",
            "survey_acronym"))

  setattr(cpi,
          "raw_sha_fun",
          raw_sha_fun)

  # validate cpi clean data before saving it
  cpi_validate_output(cpi, detail = detail)

  # Save
  if (branch == "main") {
    branch <- ""
  }
  msrdir <- fs::path(maindir, "aux_data", branch, measure) # measure dir

  saved <- pipfun::pip_sign_save(
    x       = cpi,
    measure = measure,
    msrdir  = msrdir,
    force   = force
  )


  return(invisible(saved))
}

#' Check CPI Vintage
#'
#' @param msrdir character: measure directory.
#' @param dlwdir character: Datalibweb directory
#' @param force logical: If TRUE force update of vintage level 1.
#'
#' @keywords internal
aux_cpi_vintage <- function(msrdir = fs::path(gls$PIP_DATA_DIR, "_aux/", measure),
                            dlwdir  = Sys.getenv("PIP_DLW_ROOT_DIR"),
                            force = FALSE) {
  time <- format(Sys.time(), "%Y%m%d%H%M%S") # find a way to account for time zones
  measure <- "cpi"

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #---------   Prepar3 date   ---------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # get directories
  cpi_files <- fs::dir_ls(dlwdir, regexp = "GMD_CPI\\.dta$", recurse = TRUE, type = "file")

  # load data
  last_file <- max(cpi_files)
  vintage   <- load_cpi(last_file)

  tokeep    <- names(vintage) |>
    {\(.) grep("^cpi[0-9]{4}", ., value = TRUE)}() |>
    c("code", "year", "survname", "cpi_data_level", "cpi_ppp_id")

  vintage <-  vintage[, ..tokeep]

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #---------   check version and save  ---------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  # save file
  sfile <- fs::path(msrdir, "cpi_vintage.rds")

  equal_vintage <- TRUE
  if (fs::file_exists(sfile)) {

    cfile <- readr::read_rds(sfile)
    attr(cfile, "time") <- NULL # remove attributes
    attr(cfile, "user") <- NULL # remove attributes
    cf_vt <- all.equal(cfile, vintage)

    if (inherits(cf_vt, "character")) {
      equal_vintage <- FALSE
    }
  } else {
    equal_vintage <- FALSE
  }

  if (equal_vintage == FALSE || force == TRUE) {
    attr(vintage, "time") <- time
    attr(vintage, "user") <- Sys.info()[8]

    readr::write_rds(
      x = vintage,
      file = sfile
    )
  }

  return(!equal_vintage)
} # end of vintage_level_2

#' Load cpi files and create CPI ID variable
#' @param x character: cpi file name
#' @return data frame
load_cpi <- function(x) {
  cpi_ppp_id <- gsub("(.*/Support_2005_)([^/]+)(_CPI\\.dta$)", "\\2", x)
  df <- haven::read_dta(x)
  df$cpi_ppp_id <- cpi_ppp_id

  to_keep <- c("label")

  to_keep_regx <- paste(to_keep, collapse = "|")

  nn <- names(df)
  for (x in seq_along(nn)) {
    ats <- attributes(df[[x]])
    atsn <- names(ats)
    to_remove <- atsn[!grepl(to_keep_regx, atsn)]

    for (i in seq_along(to_remove)) {
      attr(df[[x]], to_remove[i]) <- NULL
    }
  }

  data.table::setDT(df)
  return(df)
}

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
    # validate_cols(in_set(c("N", "R", "U")),
    #               survey_coverage, description = "`survey_coverage` values within range") |>
    # validate_if(is.numeric(cpi2011_SM22),
    #             description = "`cpi2011_SM22` should be numeric") |>
    validate_if(is.numeric(comparable),
                description = "`comparable` should be numeric") |>
    # validate_if(is.numeric(cpi2017_SM22),
    #             description = "`cpi2017_SM22` should be numeric") |>
    validate_cols(is.logical, cpi2005,
                  description = "`cpi2005` should be logical") |>
    validate_if(is.numeric(cpi_data_level),
                description = "`cpi_data_level` should be numeric") |>
    validate_cols(in_set(c(0, 1, 2)),
                  cpi_data_level, description = "`cpi_data_level` values within range") |>
    # validate_if(is.numeric(ref_year_SM24),
    #             description = "`ref_year_SM24` should be numeric") |>
    # validate_if(is.numeric(cpi2011_SM24),
    #             description = "`cpi2011_SM24` should be numeric") |>
    # validate_if(is.numeric(cpi2017_SM24),
    #             description = "`cpi2011_SM24` should be numeric") |>
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

#' Validate clean cpi data
#'
#' @param cpi clean cpi data, output via `aux_cpi_clean`
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#' @keywords internal
#'
#' @export
cpi_validate_output <- function(cpi, detail = getOption("pipaux.detail.output")){

  stopifnot("CPI clean data is not loaded" = !is.null(cpi))

  report <- data_validation_report()

  validate(cpi, name = "CPI output data validation") |>
    validate_if(is.character(country_code),
                description = "`country_code` should be character") |>
    validate_if(is.integer(year),
                description = "`year` should be integer") |>
    validate_if(is.numeric(survey_year),
                description = "`survey_year` should be numeric") |>
    validate_if(is.numeric(cpi),
                description = "`cpi` should be numeric") |>
    validate_if(is.numeric(ccf),
                description = "`ccf` should be numeric") |>
    validate_if(is.character(survey_acronym),
                description = "`survey_acronym` should be character") |>
    validate_if(is.numeric(change_cpi2011),
                description = "`change_cpi2011` should be numeric") |>
    validate_cols(in_set(c(0, 1)), change_cpi2011,
                  description = "`change_cpi2011` values within range") |>
    # validate_if(is.character(cpi_domain),
    #             description = "`cpi_domain` should be character") |>
    # validate_cols(in_set(c("National", "Urban/Rural")), cpi_domain,
    #               description = "`cpi_domian` values within range") |>
    validate_if(is.numeric(cpi_domain_value),
                description = "`cpi_domain_value` should be numeric") |>
    validate_cols(in_set(c(0, 1)), cpi_domain_value,
                  description = "`cpi_domain_value` values within range") |>
    validate_if(is.numeric(cpi2017_unadj),
                description = "`cpi2017_unadj` should be numeric") |>
    validate_if(is.numeric(cpi2011_unadj),
                description = "`cpi2011_unadj` should be numeric") |>
    validate_if(is.numeric(cpi2011),
                description = "`cpi2011` should be numeric") |>
    validate_if(is.numeric(cpi2017),
                description = "`cpi2017` should be numeric") |>
    # validate_if(is.numeric(cpi2011_SM22),
    #             description = "`cpi2011_SM22` should be numeric") |>
    # validate_if(is.numeric(cpi2017_SM22),
    #             description = "`cpi2017_SM22` should be numeric") |>
    validate_cols(is.logical, cpi2005,
                  description = "`cpi2005` should be logical") |>
    validate_if(is.character(reporting_level),
                description = "`reporting_level` should be character") |>
    validate_cols(in_set(c("national", "rural", "urban")), reporting_level,
                  description = "`reporting_level` values within range") |>
    # validate_if(is.numeric(cpi2011_AM23),
    #             description = "`cpi2011_AM23` should be numeric") |>
    # validate_if(is.numeric(cpi2017_AM23),
    #             description = "`cpi2017_AM23` should be numeric") |>
    validate_if(is.character(cpi_id),
                description = "`cpi_id` should be character") |>
    validate_cols(not_na, country_code, year, survey_acronym, reporting_level,
                  description = "no missing values in key variables") |>
    validate_if(is_uniq(country_code, year, survey_acronym,
                        reporting_level),
                description = "no duplicate records in key variables") |>
    validate_if(is_uniq(country_code, year, survey_acronym,
                        reporting_level),
                description = "no duplicate cpi values") |>
    add_results(report)

  num_var_list1 <- grep("cpi2011_", colnames(cpi))
  num_var_list2 <- grep("cpi2017_", colnames(cpi))
  num_var_list <- c(num_var_list1, num_var_list2)

  for (i in 1:length(num_var_list)) {
    validate(cpi, name = "CPI validation") |>
      validate_cols(is.numeric, num_var_list[i],
                    description = "variables (with numeric var name) should be numeric") |>
      add_results(report)
  }

  validation_record <- get_results(report, unnest = FALSE) |>
    setDT()

  if (any(validation_record[["type"]] == "error")){
    get_error_validation(validation_record, detail)
  }
}




