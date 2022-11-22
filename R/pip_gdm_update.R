#' Update GDM
#'
#' Update GDM data using the PovcalNet Masterfile.
#'
#' @inheritParams pip_gdm
#' @keywords internal
pip_gdm_update <- function(force = FALSE,
                           owner   = getOption("pipfun.ghowner"),
                           maindir = gls$PIP_DATA_DIR,
                           branch  = c("DEV", "PROD", "main", ""),
                           tag     = match.arg(branch)
                           ) {
  measure <- "gdm"
  branch <- match.arg(branch)

#   ____________________________________________________________________________
#   Load raw file                                                  ####

  df <- pipfun::load_from_gh(measure = "gdm",
                     owner = owner,
                     branch = branch,
                     tag = tag,
                     ext = "xlsx")


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

  pip_pfw(maindir = maindir,
          force   = force,
          owner   = owner,
          branch  = branch,
          tag     = tag)

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

  pip_country_list(maindir = maindir,
                   force   = force,
                   owner   = owner,
                   branch  = branch,
                   tag     = tag)

  cl   <- load_aux(measure = "country_list",
                   maindir = maindir,
                   branch = branch)

  df <- df[country_code %in% cl$country_code]


  # ---- Save and sign ----

  msrdir <- fs::path(maindir, "_aux", branch, measure) # measure dir
  saved <- pipfun::pip_sign_save(
    x       = df,
    measure = measure,
    msrdir  = msrdir,
    force   = force
  )
  return(invisible(saved))
}
