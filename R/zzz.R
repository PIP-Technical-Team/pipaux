
gls <- pipfun::pip_create_globals()
pipuax_default_options <- list(
  pipaux.cpivar  = "cpi2011",
  pipaux.pppvar  = "icp2011",
  pipaux.pppyear = 2011,
  pipaux.popsrc  = "emi",
  pipaux.madsrc  = "https://www.rug.nl/ggdc/historicaldevelopment/maddison/data/mpd2020.dta",
  pipaux.maindir = gls$PIP_DATA_DIR,
  pipfun.ghowner = "PIP-Technical-Team",
  joyn.verbose   = FALSE,
  pipfun.verbose = TRUE

)


dependencies <- list(ppp = "country_list",
                     pfw = character(),
                     gdp = c("weo", "maddison", "wdi", "country_list"),
                     wdi = character(),
                     weo = c("pop"),
                     pop = c("country_list", "pfw"),
                     countries = c("pfw", "country_list"),
                     metadata = "pfw",
                     gdm = c("country_list", "pfw"),
                     regions = c("country_list"),
                     maddison = character(),
                     country_list = character(),
                     pce = c("wdi", "country_list"),
                     cpi = "country_list",
                     missing_data = c("country_list", "pce", "gdp", "pop", "pfw")
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

