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
  msrdir <- paste0(maindir, "_aux/", measure, "/")

  if (action == "update") {

    # Load pfw
    pfw <- load_aux("pfw")
    pfw <- data.table::setDT(pfw)
    pfw$link <- with(
      pfw,
      sprintf(
        "%s_%s_%s",
        country_code,
        surveyid_year,
        survey_acronym
      )
    )

    # Pick the latest metadata file
    files <- list.files(paste0(maindir, "_aux/metadata/"),
      pattern = "metadata_to_update",
      full.names = TRUE
    )
    path <- sort(files, decreasing = TRUE)[1]
    df <- readxl::read_xlsx(path)

    # Merge datasets (inner join)
    df <-
      merge(df,
        pfw[, c("country_code", "surveyid_year", "survey_acronym", "link")],
        by = "link", all = FALSE
      )

    # Recode colnames
    df <- df %>%
      data.table::setnames("title", "survey_title")

    # Select columns
    df <- df[
      c(
        "country_code", "surveyid_year", "survey_acronym",
        "survey_title", "year_start", "year_end",
        "authoring_entity_name", "abstract",
        "collection_dates_cycle", "collection_dates_start",
        "collection_dates_end", "coverage",
        "sampling_procedure", "collection_mode",
        "coll_situation", "weight", "cleaning_operations"
      )
    ]
    df <- data.table::setDT(df)

    # Sign and save
    pip_sign_save(
      x = df,
      measure = "metadata",
      msrdir = msrdir,
      force = force,
      save_dta = FALSE
    )
  } else if (action == "load") {
    df <- load_aux(
      maindir = maindir,
      measure = measure
    )
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
