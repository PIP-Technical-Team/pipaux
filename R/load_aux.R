#' Load any auxiliary data
#'
#' @inheritParams load_raw_aux
#' @param apply_label logical: If TRUE, predefined labels will applied.
#' @export
load_aux <- function(measure,
                     maindir = gls$PIP_DATA_DIR,
                     branch  = c("DEV", "PROD", "main"),
                     apply_label = TRUE) {

  branch <- match.arg(branch)
  msrdir <- fs::path(maindir, "_aux/", branch, measure)

  # check file exists
  if (file.exists(fs::path(msrdir, measure, ext = "fst"))) {

    df <- fst::read_fst(fs::path(msrdir, measure, ext = "fst"), as.data.table = TRUE)

  } else if (file.exists(fs::path(msrdir, measure, ext = "rds"))) {

    df <- readRDS(fs::path(msrdir, measure, ext = "rds"))

  } else {
    msg <- sprintf("file for measure `%s` does not exist.", measure)
    rlang::abort(c(
      msg,
      i = "check your connection or data availability"
    ),
    class = "pipaux_error"
    )
  }
  if (apply_label) {
    df <- pip_aux_labels(df, measure = measure)
  }

  if (inherits(df, "data.frame")) {
    setDT(df)
  }
  return(df)
}
