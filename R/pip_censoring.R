#' Censoring data
#'
#' Load or update censoring data
#'
#'
#' @inheritParams pip_pfw
#' @inheritParams pipfun::load_from_gh
#' @export
pip_censoring  <- function(action  = c("update", "load"),
                           force   = FALSE,
                           owner   = getOption("pipfun.ghowner"),
                           maindir = gls$PIP_DATA_DIR,
                           branch  = c("DEV", "PROD", "main"),
                           tag     = match.arg(branch)) {

  measure <- "censoring"
  branch <- match.arg(branch)
  action <- match.arg(action)

  if (action == "update") {

    countries <- pipfun::load_from_gh(measure = measure,
                                      owner   = owner,
                                      branch  = branch,
                                      ext     = "xlsx",
                                      sheet   = "countries")

    regions   <- pipfun::load_from_gh(measure = measure,
                                      owner   = owner,
                                      branch  = branch,
                                      ext     = "xlsx",
                                      sheet   = "regions")

    countries[,
              id := paste(country_code, reporting_year,
                          survey_acronym, welfare_type,
                          reporting_level, sep = "_")]

    regions[,
            id := paste(region_code, reporting_year, sep = "_")]

    dl <- list(countries = countries,
               regions   = regions)

    msrdir <- fs::path(maindir, "_aux", branch, measure) # measure dir
    saved <- pipfun::pip_sign_save(
      x       = dl,
      measure = measure,
      msrdir  = msrdir,
      force   = force
    )
    return(invisible(saved))

  } else {

    dt <- load_aux(
      maindir = maindir,
      measure = measure,
      branch  = branch
    )
    return(dt)

  }
}
