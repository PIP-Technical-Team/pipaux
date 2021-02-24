pipuax_default_options <- list(
  pipaux.dlwdir  = "//wbgfscifs01/GPWG-GMD/Datalib/GMD-DLW/Support/Support_2005_CPI/",
  pipaux.maindir = "//w1wbgencifs01/pip/PIP-Data/_testing/pipdp_testing/",
  pipaux.cpivar  = "cpi2011",
  pipaux.pppvar  = "icp2011",
  pipaux.pppyear = 2011,
  pipaux.popsrc  = "emi",
  pipaux.madsrc  = "https://www.rug.nl/ggdc/historicaldevelopment/maddison/data/mpd2020.dta"
)

.onLoad <- function(libname, pkgname) {
  op <- options()
  toset <- !(names(pipuax_default_options) %in% names(op))
  if (any(toset)) options(pipuax_default_options[toset])

  invisible()
}
