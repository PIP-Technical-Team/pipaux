#' Load any auxiliary data
#'
#' @inheritParams pip_prices
#' @param apply_label logical: If TRUE, predefined labels will applied.
#' @export
load_aux <- function(measure = NULL,
                     maindir = getOption("pipaux.maindir"),
                     apply_label = TRUE) {
  msrdir <- paste0(maindir, "_aux/", measure, "/")

  if (is.null(measure)) {
    rlang::abort(c(
      "`measure` must be defined, as it does not have default value",
      i = "make sure `measure` is not NULL."
    ),
    class = "pipaux_error"
    )
  }
  # check file exists
  if (file.exists(paste0(msrdir, measure, ".fst"))) {
    df <- fst::read_fst(paste0(msrdir, measure, ".fst"), as.data.table = TRUE)
  } else if (file.exists(paste0(msrdir, measure, ".rds"))) {
    df <- readRDS(paste0(msrdir, measure, ".rds"))
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
