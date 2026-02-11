pipuax_default_options <- list(
  pipaux.cpivar        = "cpi2017",
  pipaux.pppvar        = "icp2017",
  pipaux.pppyear       = 2017,
  pipaux.popsrc        = "emi",
  pipaux.madsrc        = "https://www.rug.nl/ggdc/historicaldevelopment/maddison/data/mpd2020.dta",
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

  wrk_release <- pipfun::get_wrk_release(verbose = FALSE)
  rlang::env_poke(.pipaux, "wrk_release", wrk_release)

  # Get all pip folder paths
  pip_folders <- pipfun::get_pip_folders(verbose = FALSE)
  rlang::env_poke(.pipaux, "pip_folders", pip_folders)

  # Get all pip aliases relevant to aux data
  aux_alias <- pipfun::get_pip_aliases("aux_data", verbose = FALSE)
  rlang::env_poke(.pipaux, "aux_alias", aux_alias)
  aux_meta_alias <- pipfun::get_pip_aliases("aux_metadata", verbose = FALSE)
  rlang::env_poke(.pipaux, "aux_meta_alias", aux_meta_alias)

  if (is.null(pip_folders)) stop("Cannot find pip_folders in the environment.")
  
  # Attach relevant paths to .pipaux environment
  aux_data_path      <- pip_folders$aux_data
  aux_metadata_path  <- pip_folders$aux_metadata

  rlang::env_poke(.pipaux, "aux_data_path", aux_data_path)
  rlang::env_poke(.pipaux, "aux_metadata_path", aux_metadata_path)

  # Initialize a log
  pipfun::log_init("pipaux_update_log",
                   overwrite = TRUE)
  
  # ---- Initialize stamp defaults for pipaux ----
  pipaux_reset_stamp_options()


  invisible()
}

