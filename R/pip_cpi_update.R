#' update CPI dataframe
#'
#' @param msrdir character: measure (CPI) directory. created on `pip_prices()`.
#' @inheritParams pip_prices
#'
#' @return
#' @export
#'
#' @examples
pip_cpi_update <- function(msrdir = paste0(getOption("pipaux.maindir"), "_aux/cpi/"),
                           dlwdir = getOption("pipaux.dlwdir"),
                           force  = FALSE){

  vintage <- FALSE
  vintage <- pip_cpi_vintage(msrdir = msrdir,
                             dlwdir = dlwdir,
                             force  =  force)


  if (vintage == TRUE || force == TRUE) {

    cpi_files  <- fs::dir_ls(dlwdir,
                             regexp = "GMD_CPI\\.dta$",
                             recurse = TRUE,
                             type = "file")

    latest_cpi <- max(cpi_files)
    cpi_id     <- gsub("(.*/Support_2005_)([^/]+)(_GMD_CPI\\.dta$)", "\\2", latest_cpi)


    cpidlw     <- haven::read_dta(latest_cpi)
    cpi        <- pip_cpi_clean(cpidlw, cpi_id = cpi_id)

    pip_sign_save(x       = cpi,
                  measure = "cpi",
                  msrdir  = msrdir,
                  force   = force)
  } else {
    cli::cli_alert_success("cpi data is up to date")
    return(invisible(FALSE))
  }

}
