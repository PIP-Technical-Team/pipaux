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
    files <- list.files(fs::path(maindir, "_aux/metadata/"),
      pattern = "metadata_to_update",
      full.names = TRUE
    )
    path <- sort(files, decreasing = TRUE)[1]
    df <- readxl::read_xlsx(path)

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
        pfw[, c("country_code", "surveyid_year", "survey_acronym",
                "welfare_type", "reporting_year", "distribution_type",
                "surv_producer","survey_coverage", "surv_title",
                "link", "survey_year")],
        by = "link", all.y = TRUE
      )

    # Recode colnames
    df <- df %>%
      data.table::setnames(c("title", "surv_producer"),
                           c("survey_title", "survey_conductor"))
    df$survey_title <- ifelse(is.na(df$survey_title), df$surv_title, df$survey_title)

    # Select columns
    df <- df[
      c(
        "country_code", "reporting_year",
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
    df <- data.table::setDT(df)

    # Create nested table
    df <- tidyfast::dt_nest(df, country_code, reporting_year, survey_year,
                            survey_title, survey_conductor, survey_coverage,
                            welfare_type, distribution_type, .key = "metadata")

    # Check hash
    hash <- digest::digest(df, algo = "xxhash64")
    current_hash <- tryCatch(
      readr::read_lines(fs::path(maindir, "_aux/metadata/metadata_datasignature.txt")),
      error = function(e) NULL
    )

    # Save data
    if (hash != current_hash || force || is.null(current_hash)) {

      # Create vintage folder
      wholedir <- fs::path(maindir, "_aux/metadata/_vintage/")
      if (!(dir.exists(wholedir))) {
        dir.create(wholedir, recursive = TRUE)
      }

      # Write files
      time <- format(Sys.time(), "%Y%m%d%H%M%S")
      saveRDS(df, fs::path(maindir, "_aux/metadata/metadata.rds"))
      saveRDS(df, sprintf("%s_aux/metadata/_vintage/metadata_%s.rds", maindir, time))
      readr::write_lines(hash, file = fs::path(maindir, "_aux/metadata/metadata_datasignature.txt"))

      # Print msg
      infmsg <- paste(
        "Data signature has changed, it was not found,",
        "or update was forced.\n",
        paste0("`", "metadata", ".rds` has been updated")
      )
      rlang::inform(infmsg)
      return(invisible(TRUE))
    } else {
      rlang::inform("Data signature is up to date.\nNo update performed")
      return(invisible(FALSE))
    }

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
