#' Censoring data
#'
#' Load or update censoring data
#'
#' If `action = "update"`, the function retrieves the most recent censoring data files
#' from the GitHub repo and saves them locally
#' in the auxiliary data directory.
#'
#' If `action = "load"`, the function reads the previously saved local version of the
#' censoring data and returns it as a list of data.tables.
#'
#'
#' @inheritParams aux_pfw
#' @inheritParams pipfun::load_from_gh
#' @export
aux_censoring  <- function(action  = c("update", "load"),
                           force   = FALSE,
                           owner   = getOption("pipfun.ghowner"),
                           maindir = gls$PIP_DATA_DIR,
                           tag     = match.arg(branch)) {

  measure <- "censoring"
  action <- match.arg(action)

  pipfun::get_wrk_release(verbose = FALSE)

  release        <- wrk_release$release
  identity       <- wrk_release$identity
  branch         <- paste0(release, "_", identity)

  if (action == "update") {

    countries <- pipfun::load_from_gh(measure = measure,
                                      owner   = owner,
                                      branch  = branch,
                                      filename   = "countries.csv")

    regions   <- pipfun::load_from_gh(measure = measure,
                                      owner   = owner,
                                      branch  = branch,
                                      filename   = "regions.csv")

    countries[,
              id := paste(country_code, reporting_year,
                          survey_acronym, welfare_type,
                          reporting_level, sep = "_")]

    regions[,
            id := paste(region_code, reporting_year, sep = "_")]

    dl <- list(countries = countries,
               regions   = regions)

    if (branch == "main") {
    branch <- ""
    }
    # ----- function raw sha ----------------------

    raw_sha_fun <- digest::digest(body(
      paste0("aux_", measure))
    )


    setattr(dl,
            "raw_sha_fun",
            raw_sha_fun)

  msrdir <- fs::path(maindir, "aux_data", branch, measure) # measure dir
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
