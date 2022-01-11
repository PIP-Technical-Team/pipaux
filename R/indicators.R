#' Load raw indicators data
#'
#' @param file character: File name.
#' @param maindir character: Main directory.
#' @return list
#' @keywords internal
load_indicators <- function(file, maindir){

  assertthat::assert_that(tools::file_ext(file) == "csv",
                          msg = "File extention must be .csv.")

  mrsdir <- "indicators"
  path <- fs::path(maindir, mrsdir, file)
  df <- suppressMessages(readr::read_csv(path))

  return(df)
}

verify_input_indicators <- function(df){
  # A lot of checks here

  return(df)
}
