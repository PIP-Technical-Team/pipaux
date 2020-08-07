#' Title
#'
#' @param x Data frame to be labeled
#' @param measure type of data frame
#'
#' @return
#' @export
#'
#' @examples
pip_aux_labels <- function(x, measure) {

  if (measure == "cpi") {
    # Label variables
    attr(x$ccf,            "label") <- "Currency conversion factor"
    attr(x$reference_year, "label") <- "Proportion of first year of survey"
    attr(x$datalevel,      "label") <- "Data level to join with microdata"
    attr(x$surveyid_year,  "label") <- "Survey ID year"
    return(x)
  }

}
