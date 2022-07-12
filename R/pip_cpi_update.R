#' Update CPI
#'
#' @inheritParams pip_cpi
#' @keywords internal
pip_cpi_update <- function(maindir = gls$PIP_DATA_DIR,
                           force   = FALSE,
                           owner   = getOption("pipaux.ghowner"),
                           branch  = c("DEV", "PROD", "main"),
                           tag     = match.arg(branch)) {

#   ____________________________________________________________________________
#   Set up                                                                  ####

  branch  <- match.arg(branch)
  measure <- "cpi"


#   ____________________________________________________________________________
#   load raw data                                                           ####

  cpi <- load_raw_aux(
    measure = measure,
    owner  = owner,
    branch = branch,
    tag    = tag
  )


#   ____________________________________________________________________________
#   Cleaning                                                                ####

  # Clean data
  cpi <- pip_cpi_clean(cpi,
                       maindir = maindir,
                       branch = branch)

  # Save
  msrdir <- fs::path(maindir, "_aux", branch, measure) # measure dir
  saved <- pip_sign_save(
    x       = cpi,
    measure = measure,
    msrdir  = msrdir,
    force   = force
  )

  return(invisible(saved))
}

