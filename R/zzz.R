
# gls <- pipfun::pip_create_globals()
pipuax_default_options <- list(
  pipaux.cpivar  = "cpi2021",
  pipaux.pppvar  = "icp2021",
  pipaux.pppyear = 2021,
  pipaux.popsrc  = "emi",
  pipaux.madsrc  = "https://dataverse.nl/api/access/datafile/421303",
  # pipaux.maindir = gls$PIP_DATA_DIR,
  pipfun.ghowner = "PIP-Technical-Team",
  joyn.verbose   = FALSE,
  pipfun.verbose = TRUE,
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

