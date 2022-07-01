#' Update PPP
#'
#' @inheritParams pip_cpi
#' @keywords internal
pip_ppp_update <- function(maindir = gls$PIP_DATA_DIR,
                           force   = FALSE,
                           owner   = "PIP-Technical-Team",
                           repo    = "aux_cpi",
                           branch  = c("DEV", "PROD", "main"),
                           tag     = match.arg(branch)) {


#   ____________________________________________________________________________
#   set up                                                                  ####

  branch  <- match.arg(branch)
  measure <- "ppp"


#   ____________________________________________________________________________
#   Load raw data                                                           ####

  ppp <- load_raw_aux(
    measure = measure,
    owner  = owner,
    repo   = repo,
    branch = branch,
    tag    = tag
  )


#   ____________________________________________________________________________
#   cleaning                                                                ####


  # Clean data
  ppp <- pip_ppp_clean(ppp)

  # Remove any non-WDI countries
  cl <- load_aux(maindir = maindir,
                 measure = "country_list",
                 branch = branch)

  ppp <- ppp[country_code %in% cl$country_code]


##  ............................................................................
##  Special cases                                                           ####

  # Hardcode domain / data_level fix for NRU
  ppp$ppp_domain <-
    ifelse(ppp$country_code == "NRU" & is.na(ppp$ppp_domain),
      1, ppp$ppp_domain
    )
  ppp$ppp_data_level <-
    ifelse(ppp$country_code == "NRU" & ppp$ppp_data_level == "",
      "national", ppp$ppp_data_level
    )


#   ____________________________________________________________________________
#   Saving                                                                  ####
  msrdir <- fs::path(maindir, "_aux", branch, measure) # measure dir
  saved <- pip_sign_save(
    x       = ppp,
    measure = measure,
    msrdir  = msrdir,
    force   = force
  )


#   ____________________________________________________________________________
#   PPP vintages data                                                     ####

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

  return(invisible(saved))
}
