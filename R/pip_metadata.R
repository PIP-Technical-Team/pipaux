#' PIP Survey Metadata
#'
#' Update or load a dataset with survey metadata.
#'
#' @inheritParams pip_prices
#' @export
pip_metadata <- function(action = "update",
                         force = FALSE,
                         maindir = getOption("pipaux.maindir")) {
  measure <- "metadata"
  msrdir <- fs::path(maindir, "_aux/", measure)

  if (action == "update") {

    # Load pfw
    pfw <- load_aux("pfw", maindir = maindir)
    pfw <- data.table::setDT(pfw)

    # Pick the latest metadata file
    metadata_dir <- fs::path(maindir, "_aux/metadata/")

    u <- "https://github.com/PIP-Technical-Team/aux_metadata/raw/main/metadata.csv"
    df <- suppressMessages(
      fread(u)
    )


    # Create distribution type column (data type)
    domain_check <- with(pfw, (gdp_domain == 2 | pce_domain == 2 |
                               pop_domain == 2 | cpi_domain == 2 |
                               ppp_domain == 2))
    pfw$distribution_type <- ifelse(pfw$use_microdata == 1,
                                    "micro", NA_character_)
    pfw$distribution_type <- ifelse(pfw$use_imputed == 1,
                                    "micro, imputed",
                                    pfw$distribution_type)
    pfw$distribution_type <- ifelse(pfw$use_groupdata == 1,
                                    "group",
                                    pfw$distribution_type)
    pfw$distribution_type <- ifelse(pfw$use_groupdata == 1 & domain_check,
                                    "aggregated",
                                    pfw$distribution_type)
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

    df$survey_title <- ifelse(is.na(df$survey_title), df$surv_title, df$survey_title)

    # Select columns
    df <- df[,
      c(
        "country_code", "country_name", "reporting_year",
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
    df <- tidyfast::dt_nest(df, country_code, country_name, reporting_year, survey_year,
                            survey_title, survey_conductor, survey_coverage,
                            welfare_type, distribution_type, .key = "metadata")
    #   _________________________________________________________________________
    #   Save and Return                                                     ####

    saved <- pip_sign_save(
      x       = df,
      measure = measure,
      msrdir  = msrdir,
      force   = force,
      save_dta = FALSE
    )

    return(invisible(saved))


  } else if (action == "load") {
    df <- readRDS(fs::path(maindir, "_aux/metadata/metadata.rds"))
    return(df)
  } else {
    msg <- paste("action `", action, "` is not a valid action.")
    rlang::abort(c(
      msg,
      i = "make sure you select `update` or `load`"
    ),
    class = "pipaux_error"
    )
  }
}

