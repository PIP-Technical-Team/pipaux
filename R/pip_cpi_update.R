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
  measure <- "cpi"
  msrdir <- fs::path(maindir, "_aux/", measure) # measure dir
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
  return(cpi)

  # Save
  saved <- pip_sign_save(
    x = cpi,
    measure = "cpi",
    msrdir = msrdir,
    force = force
  )


  return(invisible(saved))
}

