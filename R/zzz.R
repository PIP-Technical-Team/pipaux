
# gls <- pipfun::pip_create_globals()
pipuax_default_options <- list(
  pipaux.cpivar  = "cpi2017",
  pipaux.pppvar  = "icp2017",
  pipaux.pppyear = 2017,
  pipaux.popsrc  = "emi",
  pipaux.madsrc  = "https://www.rug.nl/ggdc/historicaldevelopment/maddison/data/mpd2020.dta",
  # pipaux.maindir = gls$PIP_DATA_DIR,
  pipfun.ghowner = "PIP-Technical-Team",
  joyn.verbose   = FALSE,
  pipfun.verbose = TRUE,
  pipaux.detail.raw = FALSE,
  pipaux.detail.output = FALSE,
  joyn.reportvar = "report"
)

.onLoad <- function(libname, pkgname) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Options --------

  op    <- options()
  toset <- !(names(pipuax_default_options) %in% names(op))
  if (any(toset)) options(pipuax_default_options[toset])

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## defined values --------

  # pipload::add_gls_to_env()


  invisible()
}

