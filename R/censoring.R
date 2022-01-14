#' Load raw censoring data
#'
#' @param path character: Path.
#' @return list
#' @keywords internal
load_censoring <- function(path){

  assertthat::assert_that(tools::file_ext(path) == "xlsx",
                          msg = "File extention must be .xlsx.")

  sheets <- readxl::excel_sheets(path)
  dl <- vector("list", 2)
  for (i in seq_along(sheets)) {
    dl[[i]] <- readxl::read_xlsx(path, sheet = sheets[i])
  }
  names(dl) <- sheets

  return(dl)
}

#' Transform censoring
#'
#' @param dl list: Censoring object.
#' @return list
#' @keywords internal
transform_censoring <- function(dl){

  # Add id columns
  dl$countries$id <-
    with(dl$countries,
         sprintf(
           "%s_%s_%s_%s_%s",
           country_code, reporting_year,
           survey_acronym, welfare_type,
           reporting_level
         ))
  dl$regions$id <-
    with(dl$regions,
         sprintf("%s_%s",
                 region_code, reporting_year
         ))

  return(dl)
}

#' Verify input censoring
#'
#' @inheritParams transform_censoring
#' @return list
#' @keywords internal
verify_input_censoring <- function(dl){

  assertthat::assert_that(all(names(dl) == c("countries", "regions")))

  # Countries
  dl$countries %>%
    assertr::verify(assertr::has_only_names("country_code","reporting_year",
                                           "reporting_level","welfare_type",
                                           "statistic", "survey_acronym")) %>%
    assertr::verify(assertr::has_class("reporting_year", class = "numeric")) %>%
    assertr::verify(assertr::has_class("country_code", "survey_acronym",
                                       "reporting_level","welfare_type",
                                       "statistic", class = "character"))

  # Regions
  dl$regions %>%
    assertr::verify(assertr::has_only_names("region_code","reporting_year",
                                            "statistic"))

  return(dl)

}

#' Verify output censoring
#'
#' @inheritParams transform_censoring
#' @return list
#' @keywords internal
verify_output_censoring <- function(dl){

  assertthat::assert_that(all(names(dl) == c("countries", "regions")))

  # Countries
  dl$countries %>%
    assertr::verify(assertr::has_only_names("country_code","reporting_year",
                                            "reporting_level","welfare_type",
                                            "statistic", "survey_acronym", "id")) %>%
    assertr::verify(assertr::has_class("reporting_year", class = "numeric")) %>%
    assertr::verify(assertr::has_class("country_code", "survey_acronym",
                                       "reporting_level","welfare_type",
                                       "statistic", "id",
                                       class = "character"))

  # Regions
  dl$regions %>%
    assertr::verify(assertr::has_only_names("region_code","reporting_year",
                                            "statistic", "id"))

  return(dl)

}

