#' Load any auxiliary data
#'
#' @inheritParams pip_prices
#' @export
load_aux <- function(measure = NULL,
                     maindir = getOption("pipaux.maindir")) {

  msrdir <- paste0(maindir, "_aux/",  measure, "/")

  if (is.null(measure)) {

    rlang::abort(c(
                  "`measure` must be defined, as it does not have default value",
                  i = "make sure `measure` is not NULL."
                  ),
                  class = "pipaux_error"
                  )


  }
  # check file exists
  if (file.exists(paste0(msrdir, measure ,".fst"))) {

    df <- fst::read_fst(paste0(msrdir, measure ,".fst"))

  } else {
    msg <- paste("file `", measure,".fst` does not exist.")
    rlang::abort(c(
      msg,
      i = "check your connection or data availability"
    ),
    class = "pipaux_error"
    )

  }
  df <- pip_aux_labels(df, measure = measure)
  return(df)
}
