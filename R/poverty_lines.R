#' Load raw poverty lines data
#'
#' @param path character: Path
#' @return list
#' @keywords internal
load_poverty_lines <- function(path){
  assertthat::assert_that(tools::file_ext(path) == "yaml",
                          msg = "File extention must be .yaml")
  dl <- yaml::read_yaml(path)
  return(dl)
}

#' Transform poverty lines
#'
#' @param dl list: Censoring object. Output of `load_poverty_lines()`.
#' @return list
#' @keywords internal
transform_poverty_lines <- function(dl){

  pls <- seq(dl$min, dl$max, dl$increment)
  df <- data.table::data.table(
    name = as.character(pls),
    poverty_line = pls
  )
  df$is_default <- ifelse(df$name == dl$default, TRUE, FALSE)
  df$is_visible <- ifelse(df$name %in% dl$visible, TRUE, FALSE)
  df$name <- ifelse(nchar(df$name) == 3, sprintf("%s0", df$name), df$name)
  df$name <- ifelse(nchar(df$name) == 1, sprintf("%s.00", df$name), df$name)

  return(df)

}

#' Verify poverty lines input data
#'
#' Verify that the raw poverty lines data has the correct structure.
#'
#' @param dl list: Poverty lines object.
#' @keywords internal
#' @return list
verify_input_poverty_lines <- function(dl){

  assertthat::assert_that(all(names(dl) == c("default", "min", "max", "visible", "increment")))
  return(dl)

}

#' Verify poverty lines output data
#'
#' Verify that the transformed poverty lines data has the correct structure.
#'
#' @param df data.frame: Poverty lines object.
#' @keywords internal
#' @return list
verify_output_poverty_lines <- function(df){

  df %>%
    assertr::verify(assertr::has_only_names(
      "name","poverty_line", "is_default","is_visible")) %>%
    assertr::verify(assertr::has_class("poverty_line", class = "numeric")) %>%
    assertr::verify(assertr::has_class("name", class = "character")) %>%
    assertr::verify(assertr::has_class("is_default", "is_visible", class = "logical")) %>%
    assertr::verify(nchar(name) == 4) %>%
    assertr::verify(sum(is_default) == 1)

  return(df)

}

