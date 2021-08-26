#' Update PFW
#'
#' @inheritParams pip_prices
#' @export
pip_pfw_update <- function(maindir = getOption("pipaux.maindir"),
                           dlwdir = getOption("pipaux.dlwdir"),
                           force = FALSE) {
  measure <- "pfw"
  msrdir <- paste0(maindir, "_aux/", measure, "/") # measure dir
  cl <- pip_country_list("load", maindir = maindir)
  setDT(cl)

  # Check for last version in dlw
  pfw_files <- fs::dir_ls(dlwdir,
    regexp = "Survey_price_framework\\.dta$",
    recurse = TRUE,
    type = "file"
  )
  latest_pfw <- max(pfw_files)
  pfw_id <- gsub("(.*/Support_2005_)([^/]+)(/Data.*)", "\\2", latest_pfw)

  # Read data
  pfwdlw <- haven::read_dta(latest_pfw)

  # Clean data
  pfw <- pip_pfw_clean(pfwdlw, pfw_id = pfw_id)

  # Remove any non-WDI countries
  pfw <- pfw[country_code %in% cl$country_code]

  pip_sign_save(
    x = pfw,
    measure = "pfw",
    msrdir = msrdir,
    force = force
  )
}
