#' Update PPP
#'
#' @inheritParams pip_prices
#' @export
pip_ppp_update <- function(maindir = getOption("pipaux.maindir"),
                           dlwdir = getOption("pipaux.dlwdir"),
                           force  = FALSE){

  measure <- "ppp"
  msrdir <- paste0(maindir, "_aux/", measure, "/") # measure dir
  cl <- pip_country_list("load", maindir = maindir)
  setDT(cl)

  # Get list of files
  ppp_files  <- fs::dir_ls(dlwdir,
                           regexp = "pppdata_allvintages\\.dta$",
                           recurse = TRUE,
                           type = "file")

  # Latest version
  latest_ppp <- max(ppp_files)

  # Read data
  pppdlw <- haven::read_dta(latest_ppp)

  # Clean data
  ppp <- pip_ppp_clean(pppdlw)

  # Remove any non-WDI countries
  ppp <- ppp[country_code %in% cl$country_code]

  # Save
  pip_sign_save(x       = ppp,
                measure = "ppp",
                msrdir  = msrdir,
                force   = force)

}
