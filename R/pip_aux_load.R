#' Title
#'
#' @param msrdir
#' @param measure
#'
#' @return
#' @export
#'
#' @examples
pip_aux_load <- function(msrdir, measure){

  # check file exists
  if(file.exists(paste0(msrdir, measure ,".fst"))){

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
  return(df)
}
