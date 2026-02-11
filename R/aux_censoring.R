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
#' @inheritParams aux_pfw
#' @inheritParams pipfun::load_from_gh
#' @param ... Additional arguments passed to [pip_aux_save()] when `action = "update"`.
#' @export
aux_censoring  <- function(action  = c("update", "load"),
                           owner   = getOption("pipfun.ghowner"),
                           tag     = NULL,
                           ...) {

  measure <- "censoring"
  action <- match.arg(action)

  wrk_release <- get_from_auxenv(key = "wrk_release")

  release        <- wrk_release$release
  identity       <- wrk_release$identity
  branch         <- paste0(release, "_", identity)

  if (is.null(tag)) {
    tag <- paste0(release, "_", identity)
  }

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


    # Define key columns for censoring data
    key_cols <- c("countries", "regions")
    setattr(dl, "aux_key", key_cols)

    saved <- pip_aux_save(
      x        = dl,
      id       = measure,
      code     = aux_censoring,
      code_label = "aux_censoring",
      #pk       = key_cols,  rm this because of dl being list
      ...
    )

    return(invisible(saved))

  } else {

    dt <- pipload::load_aux_data(measure = measure)

    return(dt)

  }
}
