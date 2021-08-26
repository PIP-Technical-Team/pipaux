#' Update CPI
#'
#' @inheritParams pip_prices
#' @export
pip_cpi_update <- function(maindir = getOption("pipaux.maindir"),
                           dlwdir = getOption("pipaux.dlwdir"),
                           force = FALSE) {
  measure <- "cpi"
  msrdir <- paste0(maindir, "_aux/", measure, "/") # measure dir
  cl <- pip_country_list("load", maindir = maindir)
  setDT(cl)

  vintage <- FALSE
  vintage <- pip_cpi_vintage(
    msrdir = msrdir,
    dlwdir = dlwdir,
    force = force
  )


  if (vintage == TRUE || force == TRUE) {
    cpi_files <- fs::dir_ls(dlwdir,
      regexp = "GMD_CPI\\.dta$",
      recurse = TRUE,
      type = "file"
    )

    latest_cpi <- max(cpi_files)
    cpi_id <- gsub("(.*/Support_2005_)([^/]+)(_GMD_CPI\\.dta$)", "\\2", latest_cpi)

    # Read latest dataset from file
    cpidlw <- haven::read_dta(latest_cpi)

    # Clean data
    cpi <- pip_cpi_clean(cpidlw, cpi_id = cpi_id)

    # Remove any non-WDI countries
    cpi <- cpi[country_code %in% cl$country_code]

    # Save
    pip_sign_save(
      x = cpi,
      measure = "cpi",
      msrdir = msrdir,
      force = force
    )
  } else {
    cli::cli_alert_success("CPI data is up to date")
    return(invisible(FALSE))
  }
}
