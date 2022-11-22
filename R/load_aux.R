#' Load any auxiliary data
#'
#' @inheritParams pip_pfw
#' @inheritParams pipfun::load_from_gh
#' @param apply_label logical: If TRUE, predefined labels will applied.
#' @export
load_aux <- function(measure,
                     maindir = gls$PIP_DATA_DIR,
                     branch  = c("DEV", "PROD", "main", ""),
                     apply_label = TRUE) {

  branch <- match.arg(branch)
  msrdir <- fs::path(maindir, "_aux/", branch, measure)

  file_paths <- fs::dir_ls(msrdir,
                           type = "file",
                           regexp = glue("/{measure}\\."))

  fs::path_file(file_paths)


  fun_read <- list(
    qs   = function(path) qs::qread(path),
    fst  = function(path) fst::read_fst(path),
    rds  = function(path) readr::read_rds(path)
  )


  fp  <- find_path(file_paths)   # preferred file path
  ext <- fs::path_ext(fp)        # extension
  df <- fun_read[[ext]](fp)      # read file


  if (apply_label) {
    df <- pip_aux_labels(df, measure = measure)
  }

  if (inherits(df, "data.frame")) {
    setDT(df)
  }
  return(df)
}


#' Find path of file to be loaded depending on extension hierarchy
#'
#'
#' @param file_paths chracter: vector of file paths
#'
#' @return character vector of length 1 with preferred file path
find_path <- function(file_paths) {
  extensions <- fs::path_ext(file_paths)
  ext_order <- c("qs", "fst", "rds")

  f <- FALSE
  i <- 1
  while (f == FALSE) {

    ext <- ext_order[[i]]

    if(ext  %in% extensions) {
      p <- which(extensions == ext)
      f <- file_paths[[p]]
    } else {
      i <- i + 1
    }

  }

  if (f == FALSE) {
    msg     <- c(
      "File not found",
      "*" = "At least one of the following extension should be available: {.file {ext_order}}"
    )
    cli::cli_abort(msg,
                   class = "error_class")
  }

  return(f)

}

