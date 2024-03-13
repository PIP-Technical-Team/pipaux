#' Update metadata file
#'
#' @inheritParams pipfun::load_from_gh
#' @inheritParams pip_metadata
#' @return logical. TRUE if saved correctly. FALSE if error happened
#' @export
pip_metadata_update <- function(maindir = gls$PIP_DATA_DIR,
                                force = FALSE,
                                owner   = getOption("pipfun.ghowner"),
                                branch  = c("DEV", "PROD", "main"),
                                tag     = match.arg(branch),
                                detail  = getOption("pipaux.detail.raw")) {

  measure <- "metadata"
  branch <- match.arg(branch)
  #   ____________________________________________________________________________
  #   Computations                                                            ####

  df <- pipfun::load_from_gh(measure = measure,
                     owner = owner,
                     branch = branch,
                     tag = tag)

  # validate raw metdata data
  metadata_validate_raw(metadata = df, detail = detail)

  # Load pfw
  pfw <- load_aux(measure = "pfw",
                  maindir = maindir,
                  branch = branch)



  # Create distribution type column (data type)

  pfw[,
      domain_check := (gdp_domain == 2 | pce_domain == 2 |
                         pop_domain == 2 | cpi_domain == 2 |
                         ppp_domain == 2)]

  # order  matters here
  pfw[,
      distribution_type  := fcase(
        use_imputed   == 1, "micro, imputed",
        use_microdata == 1, "micro",
        use_groupdata == 1 & domain_check, "aggregated",
        use_groupdata == 1, "group",
        default = NA_character_
      )
  ]

  # Merge datasets (inner join)
  df <-
    merge(df,
          pfw[, c("country_code", "ctryname", "surveyid_year", "survey_acronym",
                  "welfare_type", "reporting_year", "distribution_type",
                  "surv_producer","survey_coverage", "surv_title",
                  "link", "survey_year")],
          by = "link", all.y = TRUE
    )

  # Recode colnames
  setnames(x = df,
           old = c("title", "surv_producer", "ctryname"),
           new = c("survey_title", "survey_conductor", "country_name"))
  df[,
     survey_title := fifelse(is.na(survey_title), surv_title, survey_title)
  ]

  # Select columns
  df <- df[,
           c(
             "country_code",  "country_name", "reporting_year",
             "surveyid_year", "survey_year", "survey_acronym",
             "survey_conductor", "survey_coverage",
             "welfare_type", "distribution_type",
             "survey_title", "year_start", "year_end",
             "authoring_entity_name", "abstract",
             "collection_dates_cycle", "collection_dates_start",
             "collection_dates_end",
             "sampling_procedure", "collection_mode",
             "coll_situation", "weight", "cleaning_operations"
           )
  ]

  # Create nested table

  df <- df[, .(.(.SD)),
           keyby =  .(
             country_code,
             country_name,
             reporting_year,
             survey_year,
             surveyid_year,
             survey_title,
             survey_conductor,
             survey_coverage,
             welfare_type,
             distribution_type
           )
           ]

  setnames(df, old = "V1", new = "metadata")

##  ............................................................................
##  Save                                                                    ####
  # validate raw metdata data
  metadata_validate_output(metadata = df, detail = detail)

  if (branch == "main") {
    branch <- ""
  }
  msrdir <- fs::path(maindir, "_aux", branch, measure) # measure dir
  saved <- pipfun::pip_sign_save(
    x       = df,
    measure = measure,
    msrdir  = msrdir,
    force   = force
  )

  #   ____________________________________________________________________________
  #   Return                                                                  ####
  return(invisible(saved))

}
