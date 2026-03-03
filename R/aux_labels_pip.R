#' PIP Auxiliary Labels
#'
#' @param x Data frame to be labeled.
#' @param measure type of data frame, e.g., "cpi" or "PPP".
#'
#' @keywords internal
aux_labels_pip <- function(x, measure, verbose = FALSE) {

  if (measure == "cpi") {

    attr(x$ccf, "label") <- "Currency conversion factor"

  } else if (measure == "ppp") {

    ppp_year <- unique(x[x$ppp_default == TRUE, "ppp_year"])

    # Label variables

    attr(x$ppp, "label") <- paste0(
      "Purchasing Power Parity (",
      ppp_year, "2011 ICP round)"
    )
    attr(x$ppp_year, "label") <- "ICP round year "
    attr(x$release_version, "label") <- "Release version of ICP round"
    attr(x$adaptation_version, "label") <- "Adaptation version of release"
    attr(x$ppp_default, "label") <- "PPP version used by default"
  }

  else if (measure == "maddison") {

    # Label Variables
    attr(x$country_code, "label") <- "Country code"
    attr(x$year, "label") <- "Year"
    attr(x$mpd_gdp, "label") <- "GDP per capita in 2011US$, 2011 benchmark (Maddison)"

  } else if (measure == "gdp") {
    # Label Variables
    attr(x$country_code, "label") <- "Country code"
    attr(x$year, "label") <- "Year"

    attr(x$gdp, "label") <- "GDP per capita (constant 2010 US$)"
  } else if (measure == "pce") {
    attr(x$country_code, "label") <- "Country code"
    attr(x$year, "label") <- "Year"
    attr(x$pce, "label") <- "Households and NPISHs Final consumption expenditure per capita (constant 2010 US$)"
  } else if (measure == "pop") {
    attr(x$country_code, "label") <- "Country code"
    attr(x$year, "label") <- "Year"
    attr(x$pop, "label") <- "Population"
  } else {
    if (verbose) cli::cli_inform("no labels available for measure {.code {measure}}")
  }

  return(x)
}
