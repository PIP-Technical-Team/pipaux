load_regions <- function(path){
  assertthat::assert_that(tools::file_ext(path) == "csv",
                          msg = "File extention must be .csv.")
  df <- suppressMessages(readr::read_csv(path))
  return(df)
}

verify_input_regions <- function(df){

}
