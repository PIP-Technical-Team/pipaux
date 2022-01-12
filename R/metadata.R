load_metadata <- function(dir, pattern = "metadata_to_update"){
  # Pick the latest metadata file
  files <- list.files(dir, pattern, full.names = TRUE)
  path <- sort(files, decreasing = TRUE)[1]
  df <- readxl::read_xlsx(path)
  return(df)
}

transform_metadata <- function(df, pfw){

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

  # Create distribution type column (data type)
  domain_check <- with(pfw, (gdp_domain == 2 | pce_domain == 2 |
                               pop_domain == 2 | cpi_domain == 2 |
                               ppp_domain == 2))
  pfw$distribution_type <- ifelse(pfw$use_imputed == 1,
                                  "imputed", NA_character_)
  pfw$distribution_type <- ifelse(pfw$use_microdata == 1,
                                  "micro", pfw$distribution_type)
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
                  "surv_producer",
                  "link")],
          by = "link", all = FALSE
    )


  # Recode colnames
  df <- df %>%
    data.table::setnames(c("title", "surv_producer", "coverage"),
                         c("survey_title", "survey_conductor", "survey_coverage"))

  # Select columns
  df <- df[
    c(
      "country_code", "reporting_year",
      "surveyid_year", "survey_acronym",
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
  df <- tidyfast::dt_nest(df, country_code, reporting_year, survey_title,
                          survey_conductor, survey_coverage, welfare_type,
                          distribution_type, .key = "metadata")

  return(df)
}

#' Verify input metadata
#'
#' @inheritParams transform_metadata
#' @return list
#' @keywords internal
verify_input_metadata <- function(df){

}

#' Verify output metadata
#'
#' @inheritParams transform_metadata
#' @return list
#' @keywords internal
verify_output_metadata <- function(df){

}
