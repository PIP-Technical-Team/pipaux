#' Update CPI
#'
#' @inheritParams pip_cpi
#' @keywords internal
pip_cpi_update <- function(maindir = gls$PIP_DATA_DIR,
                           force   = FALSE,
                           owner   = getOption("pipfun.ghowner"),
                           branch  = c("DEV", "PROD", "main"),
                           tag     = match.arg(branch),
                           detail  = getOption("pipaux.detail.raw")) {

#   ____________________________________________________________________________
#   Set up                                                                  ####

  measure <- "cpi"
  branch  <- match.arg(branch)


#   ____________________________________________________________________________
#   load raw data                                                           ####

  cpi <- pipfun::load_from_gh(
    measure = measure,
    owner  = owner,
    branch = branch,
    tag    = tag,
    ext    = "csv"
  )

  # validate cpi raw data
  cpi_validate_raw(cpi, detail = detail)

#   ____________________________________________________________________________
#   Cleaning                                                                ####

  # Clean data
  cpi <- pip_cpi_clean(cpi,
                       maindir = maindir,
                       branch = branch)

  # changae cpi_year and cpi_data_level to year and reporting_level
  cpi <- cpi |> setnames(c("cpi_year", "cpi_data_level"),
                         c("year", "reporting_level"),
                         skip_absent=TRUE)

  setattr(cpi, "aux_name", "cpi")
  setattr(cpi,
          "aux_key",
          c("country_code", "year", "reporting_level", "survey_acronym"))

  # validate cpi clean data before saving it
  cpi_validate_output(cpi, detail = detail)

  # Save
  if (branch == "main") {
    branch <- ""
  }
  msrdir <- fs::path(maindir, "_aux", branch, measure) # measure dir

  saved <- pipfun::pip_sign_save(
    x       = cpi,
    measure = measure,
    msrdir  = msrdir,
    force   = force
  )

  return(invisible(saved))
}

