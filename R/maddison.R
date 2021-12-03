#' Fetch Maddison data
#'
#' @param url URL to the `.dta` file on www.rug.nl.
#'
#' @examples
#' u <- "https://www.rug.nl/ggdc/historicaldevelopment/maddison/data/mpd2020.dta"
#' df <- fetch_maddison(u)
#' @keywords internal
#' @return data.frame
fetch_maddison <- function(url){
  haven::read_dta(url)
}

#' Verify Maddison input data
#'
#' Verify that the raw Maddison data has the correct structure.
#'
#' @param df data.frame: Output of `fetch_maddison()`.
#' @keywords internal
#' @return data.frame
verify_input_maddison <- function(df){
  df %>%
    #assertr::verify(ncol(.) == 5) %>%
    assertr::verify(assertr::has_all_names('countrycode','country','year','gdppc','pop')) %>% # has_only_names
    assertr::verify(assertr::has_class("year", "gdppc", "pop", class = "numeric")) %>%
    assertr::verify(assertr::has_class('countrycode','country', class = "character")) %>%
    assertr::assert(assertr::within_bounds(0, Inf, include.lower = FALSE, allow.na = TRUE), gdppc,
                    error_fun = assertr::just_warn)

}

#' Transform Maddison data
#'
#' @inheritParams verify_input_maddison
#' @keywords internal
#' @return data.frame
transform_maddison <- function(df){

  # Remove data prior to 1960
  df <- df[df$year >= 1960, ]

  # Remove observations w/ missing GDP
  df <- df[!is.na(df$gdppc),]

  # Remove observations w/ negative or zero GDP
  df <- df[!df$gdppc <= 0,]

  # Recode names
  df <- data.table::setnames(df,
                             old = c("countrycode", "country", "gdppc"),
                             new = c("country_code", "country_name", "mpd_gdp")
  )

  # Keep relevant variables
  df <- df[c('country_code', 'country_name', 'mpd_gdp')]

  return(df)
}

#' Verify Maddison output data
#'
#' Verify that the transformed Maddison data has the correct structure and content.
#'
#' @param df data.frame: Output of `transform_maddison()`.
#' @keywords internal
#' @return data.frame
verify_output_maddison <- function(df, start_year = 1960, end_year) {
  df %>%
    assertr::verify(assertr::has_only_names("country_code", "country_name", "mpd_gdp")) %>%
    assertr::verify(assertr::has_class("mpd_gdp", class = "numeric")) %>%
    assertr::verify(assertr::has_class("country_code", "country_name", class = "character")) %>%
    assertr::assert(assertr::within_bounds(0, Inf, include.lower = FALSE, allow.na = FALSE), mpd_gdp) %>%
    assertr::assert(assertr::within_bounds(start_year, end_year, allow.na = FALSE), year)
}
