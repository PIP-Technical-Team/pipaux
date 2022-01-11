#' Load raw indicators data
#'
#' @param path character: Path.
#' @return list
#' @keywords internal
load_indicators <- function(path){
  assertthat::assert_that(tools::file_ext(path) == "csv",
                          msg = "File extention must be .csv.")
  df <- suppressMessages(readr::read_csv(path))
  return(df)
}

verify_input_indicators <- function(df){
  # A lot of checks here

  return(df)
}
