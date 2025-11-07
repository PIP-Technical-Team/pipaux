pipuax_default_options <- list(
  pipaux.cpivar        = "cpi2017",
  pipaux.pppvar        = "icp2017",
  pipaux.pppyear       = 2017,
  pipaux.popsrc        = "emi",
  pipaux.madsrc        = "https://www.rug.nl/ggdc/historicaldevelopment/maddison/data/mpd2020.dta",
  # pipaux.key_vars      = c("country_code", "year", "reporting_level",
  #                        "survey_acronym", "welfare_type"),
  pipfun.ghowner       = "PIP-Technical-Team",
  joyn.verbose         = FALSE,
  pipfun.verbose       = TRUE,
  pipaux.detail.raw    = FALSE,
  pipaux.detail.output = FALSE,
  pipfun.verbose       = FALSE,
  joyn.reportvar       = ".joyn"

)

.onLoad <- function(libname, pkgname) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Options --------

  op    <- options()
  toset <- !(names(pipuax_default_options) %in% names(op))

  if (any(toset)) options(pipuax_default_options[toset])

  # Copy the pins_board from pipenv environment to pipaux environment
  br <- pipfun::get_from_pipenv("pins_boards")

  # pipfun::setup_working_release(release = "20250203")
  wrk_release <- pipfun::get_wrk_release(verbose = FALSE)


  if (is.null(br)) stop("Cannot find pins_boards in the environment.")

  abr <- pipfun::get_pins_boards("aux_data")
  rlang::env_poke(.pipaux, "aux_data_board", abr)

  ameta_br <- pipfun::get_pins_boards("aux_metadata")

  rlang::env_poke(.pipaux, "aux_metadata_board", ameta_br)
  rlang::env_poke(.pipaux, "wrk_release", wrk_release)

  # Initialize a log
  pipfun::log_init("pipaux_update_log",
                   overwrite = TRUE)


  invisible()
}

