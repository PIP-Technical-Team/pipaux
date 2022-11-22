#' Update PPP
#'
#' @inheritParams pipfun::load_from_gh
#' @keywords internal
pip_ppp_update <- function(maindir = gls$PIP_DATA_DIR,
                           force = FALSE,
                           owner   = getOption("pipfun.ghowner"),
                           branch  = c("DEV", "PROD", "main", ""),
                           tag     = match.arg(branch)) {


#   ____________________________________________________________________________
#   set up                                                                  ####

  measure <- "ppp"
  branch  <- match.arg(branch)


#   ____________________________________________________________________________
#   Load raw data                                                           ####

  ppp <- pipfun::load_from_gh(
    measure = measure,
    owner  = owner,
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
  saved <- pipfun::pip_sign_save(
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
  pipfun::pip_sign_save(
    x = ppp_vintage,
    measure = "ppp_vintage",
    msrdir = msrdir,
    force = force
  )

  return(invisible(saved))
}
