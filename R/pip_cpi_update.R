#' Update CPI
#'
#' @inheritParams pip_cpi
#' @keywords internal
pip_cpi_update <- function(maindir = gls$PIP_DATA_DIR,
                           force   = FALSE,
                           owner   = "PIP-Technical-Team",
                           repo    = "aux_cpi",
                           branch  = c("DEV", "PROD", "main"),
                           tag     = match.arg(branch)) {

  branch  <- match.arg(branch)
  measure <- "cpi"
  cl <- pip_country_list("load", maindir = maindir)
  setDT(cl)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Special national accounts --------
  cpi <- load_raw_aux(
    measure = measure,
    owner  = owner,
    repo   = repo,
    branch = branch,
    tag    = tag
  )


  # Clean data
  cpi <- pip_cpi_clean(cpi)

  # Remove any non-WDI countries
  cpi <- cpi[country_code %in% cl$country_code]

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

