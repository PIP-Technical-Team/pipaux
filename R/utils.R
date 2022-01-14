#' Find latest file
#' @param dir Directory
#' @param pattern File pattern to look for. Can be a regular expression
get_latest_file <- function(dir, pattern){
  files <- fs::dir_ls(dir, regexp = pattern, recurse = TRUE, type = "file")
  return(max(files))
}
