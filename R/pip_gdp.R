#' Update and load GDP data in PIP Auxiliary data structure
#'
#' @param action
#'
#' @return
#' @export
#'
#' @examples
pip_gdp <- function(action = "update") {
  mad <- haven::read_dta("https://www.rug.nl/ggdc/historicaldevelopment/maddison/data/mpd2018.dta")
  gdp <- wbstats::wb_data(indicator = "NY.GDP.PCAP.KD")

}



library(WDI)
library(wbstats)


bench <- microbenchmark::microbenchmark(
  times = 25,
  wbs = wbstats::wb_data(indicator = "NY.GDP.PCAP.KD"),
  wdi = WDI(indicator = "NY.GDP.PCAP.KD")
)
if (requireNamespace("highcharter")) {
  highcharter::hcboxplot(bench[["time"]], bench[["expr"]], outliers = FALSE)
} else {
  boxplot(bench, outline = FALSE)
}




mpd <- haven::read_dta("https://www.rug.nl/ggdc/historicaldevelopment/maddison/data/mpd2018.dta")
r <- pip_aux_values()
msrdir <- paste0(r$maindir, "_aux/", measure, "/") # measure dir
