
gls <-   pipfun::pip_create_globals()
# gls <- pipload::pip_create_globals()
pipuax_default_options <- list(
  pipaux.cpivar  = "cpi2011",
  pipaux.pppvar  = "icp2011",
  pipaux.pppyear = 2011,
  pipaux.popsrc  = "emi",
  pipaux.madsrc  = "https://www.rug.nl/ggdc/historicaldevelopment/maddison/data/mpd2020.dta",
  pipaux.maindir = gls$PIP_DATA_DIR,
  joyn.verbose   = FALSE

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

