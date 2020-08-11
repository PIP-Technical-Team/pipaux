#' Title
#'
#' @param x Data frame to be labeled.
#' @param measure type of data frame, e.g., "cpi" or "PPP".
#'
#' @return
#' @export
#'
#' @examples
pip_aux_labels <- function(x, measure) {

  if (measure == "cpi") {

    # Label variables
    attr(x$reference_year, "label") <- "Proportion of first year of survey"
    attr(x$cpi_domain,     "label") <- "CPI domain to join with microdata"
    attr(x$cpi_data_level, "label") <- "Values to use as keys to join with cpi_domain_var"
    attr(x$surveyid_year,  "label") <- "Year of survey ID"
    attr(x$ccf,            "label") <- "Currency conversion factor"
    attr(x$cpi,            "label") <- "Consumer Price Index (Based on 2011)."

    return(x)
  }  else if (measure == "ppp") {

    # Label variables
    attr(x$reference_year, "label") <- "Proportion of first year of survey"
    attr(x$ppp_domain,     "label") <- "PPP domain to join with microdata"
    attr(x$ppp_data_level, "label") <- "Values to use as keys to join with ppp_domain_var"
    attr(x$surveyid_year,  "label") <- "Year of survey ID"
    attr(x$ppp,  "label") <- "Purchasing Power Parity (2011 ICP round)"

    return(x)

  } else {
    return(x)
  }

}



