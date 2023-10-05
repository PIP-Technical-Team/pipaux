#' Update PPP
#'
#' @inheritParams pip_prices
#' @keywords internal
pip_ppp_update <- function(maindir = gls$PIP_DATA_DIR,
                           dlwdir  = Sys.getenv("PIP_DLW_ROOT_DIR"),
                           force = FALSE) {
  measure <- "ppp"
  msrdir <- fs::path(maindir, "_aux/", measure) # measure dir
  cl <- pip_country_list("load", maindir = maindir)
  setDT(cl)

  # Get list of files
  ppp_files <- fs::dir_ls(dlwdir,
    regexp = "pppdata_allvintages\\.dta$",
    recurse = TRUE,
    type = "file"
  )

  # Latest version
  latest_ppp <- max(ppp_files)

  # Read data
  pppdlw <- haven::read_dta(latest_ppp)

  # Clean data
  ppp <- pip_ppp_clean(pppdlw)

  # Remove any non-WDI countries
  ppp <- ppp[country_code %in% cl$country_code]

  # Hardcode domain / data_level fix for NRU
  ppp$ppp_domain <-
    ifelse(ppp$country_code == "NRU" & is.na(ppp$ppp_domain),
      1, ppp$ppp_domain
    )
  ppp$ppp_data_level <-
    ifelse(ppp$country_code == "NRU" & ppp$ppp_data_level == "",
      "national", ppp$ppp_data_level
    )

  # Save
  pip_sign_save(
    x = ppp,
    measure = "ppp",
    msrdir = msrdir,
    force = force
  )

  vars        <- c("ppp_year", "release_version", "adaptation_version")
  ppp_vintage <- unique(ppp[, ..vars], by = vars)

  data.table::setnames(x = ppp_vintage,
                       old = c("release_version", "adaptation_version"),
                       new = c("ppp_rv", "ppp_av"))
  # Save
  pip_sign_save(
    x = ppp_vintage,
    measure = "ppp_vintage",
    msrdir = msrdir,
    force = force
  )


}
