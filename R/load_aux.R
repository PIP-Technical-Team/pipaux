#' Load any auxiliary data
#'
#' @inheritParams pip_prices
#' @param apply_label logical: If TRUE, predefined labels will applied.
#' @export
load_aux <- function(measure = NULL,
                     maindir = gls$PIP_DATA_DIR,
                     apply_label = TRUE) {
  msrdir <- fs::path(maindir, "_aux/", measure)

  if (is.null(measure)) {
    rlang::abort(c(
      "`measure` must be defined, as it does not have default value",
      i = "make sure `measure` is not NULL."
    ),
    class = "pipaux_error"
    )
  }
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
  return(df)
}
