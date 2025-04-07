#' PIP GDM
#'
#' Load or update grouped data means dataset from PovcalNet Masterfile. See
#' details.
#'
#' Survey means cannot be automatically calculated for grouped data, so at some
#' stage the mean needs to be entered manually. This function reads from the PCN
#' Masterfile to ensure that PCN and PIP uses the same data means.
#'
#' The dependency on the PCN Masterfile should be changed in the future.
#'
#' @inheritParams aux_cpi
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @inheritParams pipfun::load_from_gh
#' @export
aux_gdm <- function(action  = c("update", "load"),
                    force   = FALSE,
                    owner   = getOption("pipfun.ghowner"),
                    maindir = gls$PIP_DATA_DIR,
                    branch = paste0(wrk_release$release, "_", wrk_release$identity),
                    tag     = branch,
                    detail  = getOption("pipaux.detail.raw")) {

  measure <- "gdm"
  action <- match.arg(action)

  if (action == "update") {

    aux_gdm_update(force   = force,
                   maindir = maindir,
                   owner   = owner,
                   branch  = branch,
                   tag     = tag,
                   detail  = detail)

  } else {
    dt <- load_aux(
      maindir = maindir,
      measure = measure,
      branch  = branch
    )
    return(dt)
  }
}

#' Update GDM
#'
#' Update GDM data using the PovcalNet Masterfile.
#'
#' @inheritParams aux_gdm
#' @keywords internal
aux_gdm_update <- function(force = FALSE,
                           owner   = getOption("pipfun.ghowner"),
                           maindir = gls$PIP_DATA_DIR,
                           branch,
                           tag     = branch,
                           detail  = getOption("pipaux.detail.raw")) {
  measure <- "gdm"

  #   _________________________________________________________
  #   Load raw file                               ####

  df <- pipfun::load_from_gh(measure = "gdm",
                             owner  = owner,
                             branch = branch,
                             tag    = tag,
                             ext    = "csv")

  # save attributes before they get removed by subsequent formatting ops
  gh <- attr(df, "gh")

  # validate gdm raw data
  gdm_validate_raw(gdm = df, detail = detail)

  #   ____________________________________________________________________________
  #   Transform dataset                                                       ####

  # Select for grouped data surveys
  df <- df[grepl("[.]T0[1,2,5]$",
                 df$DistributionFileName,
                 ignore.case = TRUE), ]

  # Select and rename columns
  old_nms <-  c(
    "CountryCode",
    "SurveyTime",
    "DataType",
    "Coverage",
    "SurveyMean_LCU",
    "DistributionFileName",
    "SurveyID"
  )

  new_nms <- c(
    "country_code",
    "survey_year",
    "welfare_type",
    "pop_data_level",
    "survey_mean_lcu",
    "pcn_source_file",
    "pcn_survey_id"
  )

  setnames(df, old_nms, new_nms)

  df <- df[, ..new_nms]

  # Recode columns
  df[,
     c("pop_data_level", "welfare_type", "survey_coverage") :=
       {
         x <- tolower(pop_data_level)

         y <- tolower(welfare_type)
         y <- fifelse(y == "x", "consumption", "income")

         z <- fifelse(country_code %in% c("CHN", "IDN", "IND"),
                      "national", pop_data_level)

         list(x, y, z)
       }
  ]


  df[,
     distribution_type := fifelse(pop_data_level == "national",
                                  "group",
                                  "aggregate")
  ][,
    gd_type := sub(".*[.]", "", pcn_source_file)
  ]


  ##  ............................................................................
  ##  Merge with PFW                                                          ####

  pfw    <-  load_aux(measure = "pfw",
                      maindir = maindir,
                      branch = branch)
  # Subset columns
  pfw <-
    pfw[, c(
      "country_code",
      "welfare_type",
      "surveyid_year",
      "survey_year",
      "survey_acronym",
      "inpovcal"
    )]

  # Merge to add surveyid_year
  tmp <- pfw[, c("country_code", "surveyid_year", "survey_year")]
  df <- merge(df, tmp,
              all.x = TRUE,
              by = c("country_code", "survey_year")
  )

  # Merge to add survey_acronym and inpovcal
  df <- merge(df, pfw,
              all.x = TRUE,
              by = c(
                "country_code", "surveyid_year",
                "survey_year", "welfare_type"
              )
  )

  # Filter to select surveys in PovcalNet
  df <- df[inpovcal == 1]
  df <- na.omit(df, "inpovcal")


  ##  ............................................................................
  ##  Merge with inventory                                                    ####

  inv <- fst::read_fst(fs::path(maindir, "_inventory/inventory.fst"),
                       as.data.table = TRUE)

  # inv <- fst::read_fst(fs::path("Y:\\tefera_pipaux_test",
  #                               "_inventory/inventory.fst"),
  #                      as.data.table = TRUE)

  # Create survey_id column
  inv[,
      survey_id := sub("[.]dta", "", filename)
  ][,
    surveyid_year := as.numeric(surveyid_year)
  ]

  # Subset GD rows
  inv <- inv[module == "PC-GROUP"]

  # Subset columns
  inv <- inv[, c("country_code",
                 "surveyid_year",
                 "survey_acronym",
                 "survey_id")]

  # Merge to add PIP survey_id
  df <- merge(df, inv,
              all.x = TRUE,
              by = c(
                "country_code", "surveyid_year",
                "survey_acronym"
              )
  )


  # ---- Finalize table ----

  # Select columns
  df <- df[, c(
    "country_code",
    "surveyid_year",
    "survey_year",
    "welfare_type",
    "survey_mean_lcu",
    "distribution_type",
    "gd_type",
    "pop_data_level",
    "pcn_source_file",
    "pcn_survey_id",
    "survey_id"
  )]

  df[, survey_id := toupper(survey_id)]

  # Convert LCU means to daily values
  # df$survey_mean_lcu <- df$survey_mean_lcu * (12/365)

  # Sort rows
  setorder(df, country_code, surveyid_year, pop_data_level)

  # Sort columns
  setcolorder(df, "survey_id")



  ##  ............................................................................
  ##  Remove any non-WDI countries                                            ####

  aux_country_list(maindir = maindir,
                   force   = force,
                   branch  = branch)

  cl   <- load_aux(measure = "country_list",
                   maindir = maindir,
                   branch = branch)

  df <- df[country_code %in% cl$country_code]


  # ---- Save and sign ----
  df <- df |> setnames(c("surveyid_year", "pop_data_level"),
                       c("year", "reporting_level"),
                       skip_absent=TRUE)

  setattr(df, "aux_name", "gdm")
  setattr(df,
          "aux_key",
          c("country_code", "year", "reporting_level", "welfare_type"))

  # validate gdm output data
  gdm_validate_output(gdm = df, detail = detail)

  # Raw attributes ####
  # -------- From github  ----------------------

  setattr(df, "gh", gh)

  # ----- function raw sha ----------------------

  raw_sha_fun <- digest::digest(body(
    paste0("aux_", measure))
  )


  setattr(df,
          "raw_sha_fun",
          raw_sha_fun)


  if (branch == "main") {
    branch <- ""
  }
  msrdir <- fs::path(maindir, "aux_data", branch, measure) # measure dir

  ##  ----------------------------------------------------------
  ##  Save file                                           ####

  saved <- pipfun::pip_sign_save(
    x       = df,
    measure = measure,
    msrdir  = msrdir,
    force   = force
  )
  return(invisible(saved))
}

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

#' Validate clean gdm data
#'
#' @param gdm clean gdm data, output via `pipfun::pip_gdm_clean`
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#' @keywords internal
#'
#' @export
gdm_validate_output <- function(gdm, detail = getOption("pipaux.detail.output")){

  stopifnot("GDM output data is not loaded" = !is.null(gdm))

  report <- data_validation_report()

  validate(gdm, name = "GDM output data validation") |>
    validate_if(is.character(survey_id),
                description = "`survey_id` should be character") |>
    validate_if(is.character(country_code),
                description = "`country_code` should be character") |>
    validate_if(is.integer(year),
                description = "`year` should be integer") |>
    validate_if(is.numeric(survey_year),
                description = "`survey_year` should be numeric") |>
    validate_if(is.character(welfare_type),
                description = "`welfare_type` should be character") |>
    validate_cols(in_set(c("consumption", "income")), welfare_type,
                  description = "`welfare_type` values within range") |>
    validate_if(is.numeric(survey_mean_lcu),
                description = "`survey_mean_lcu` should be numeric") |>
    validate_if(is.character(distribution_type),
                description = "`distribution_type` should be character") |>
    validate_cols(in_set(c("aggregate", "group")), distribution_type,
                  description = "`distribution_type` values within range") |>
    validate_if(is.character(gd_type),
                description = "`gd_type` should be character") |>
    validate_if(is.character(reporting_level),
                description = "`reporting_level` should be character") |>
    validate_cols(in_set(c("national", "rural", "urban")), reporting_level,
                  description = "`reporting_level` values within range") |>
    validate_if(is.character(pcn_source_file),
                description = "`pcn_source_file` should be character") |>
    validate_if(is.character(pcn_survey_id),
                description = "`pcn_survey_id` should be character") |>
    validate_cols(not_na, country_code, year, reporting_level,
                  description = "no missing values in key variables") |>
    validate_if(is_uniq(country_code, year, reporting_level),
                description = "no duplicate records in key variables") |>
    add_results(report)

  validation_record <- get_results(report, unnest = FALSE) |>
    setDT()

  if (any(validation_record[["type"]] == "error")){
    get_error_validation(validation_record, detail)
  }
}

