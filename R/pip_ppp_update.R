#' Update PPP
#'
#' @inheritParams pipfun::load_from_gh
#' @keywords internal
pip_ppp_update <- function(maindir = gls$PIP_DATA_DIR,
                           force = FALSE,
                           owner   = getOption("pipfun.ghowner"),
                           branch  = c("DEV", "PROD", "main"),
                           tag     = match.arg(branch),
                           detail  = getOption("pipaux.detail.raw")) {


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

  # validate ppp raw data
  ppp_validate_raw(ppp = ppp, detail = detail)

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

  # validate ppp output data
  ppp_validate_output(ppp = ppp, detail = detail)

  if (branch == "main") {
    branch <- ""
  }

  msrdir <- fs::path(maindir, "_aux", branch, measure) # measure dir

  ppp <- ppp |> setnames("ppp_data_level", "reporting_level",
                         skip_absent=TRUE)

  setattr(ppp, "aux_name", "ppp")
  setattr(ppp,
          "aux_key",
          c("country_code", "reporting_level")) # this is going to be key variables only when PPP default year selected.

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

  ppp_vintage <- ppp_vintage |> setnames("ppp_data_level", "reporting_level",
                         skip_absent=TRUE)

  setattr(ppp_vintage, "aux_name", "ppp")
  setattr(ppp_vintage,
          "aux_key",
          c("country_code", "reporting_level"))

  # Save
  pipfun::pip_sign_save(
    x = ppp_vintage,
    measure = "ppp_vintage",
    msrdir = msrdir,
    force = force
  )

  return(invisible(saved))
}
