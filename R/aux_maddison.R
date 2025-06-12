#' Maddison data
#'
#' Load or update data from the Maddison project.
#'
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @inheritParams aux_pfw
#' @inheritParams pipfun::load_from_gh
#' @export
#' @import data.table
aux_maddison <- function(action = c("update", "load"),
                         owner   = getOption("pipfun.ghowner"),
                         force = FALSE,
                         maindir = getOption("pipaux.working_dir"),
                         tag     = NULL,
                         detail  = getOption("pipaux.detail.raw")) {
  measure <- "maddison"
  action  <- match.arg(action)

  pipfun::get_wrk_release(verbose = FALSE)

  release        <- wrk_release$release
  identity       <- wrk_release$identity
  branch         <- paste0(release, "_", identity)

  if (is.null(tag)) {
    tag <- paste0(release, "_", identity)
  }

  if (action == "update") {
    mpd <-  pipfun::load_from_gh(
      measure = measure,
      owner  = owner,
      branch = branch,
      tag    = tag,
      ext    = "csv"
    )
  # validate raw data
    mpd_validate_raw(mpd = mpd, detail = detail)

  # # validate output data
  #   mpd_validate_output(mpd)

  if (branch == "main") {
    branch <- ""
  }
  msrdir <- fs::path(maindir, "aux_data", branch, measure) # measure dir

  # ----- function raw sha ------
  raw_sha_fun <- digest::digest(body(
    aux_maddison)
  )

  setattr(mpd, "aux_name", "maddison")

  setattr(mpd,
          "aux_key",
          c("country_code", "year"))

  setattr(mpd,
          "raw_sha_fun",
          raw_sha_fun)

    saved <- pipfun::pip_sign_save(
      x = mpd,
      measure = measure,
      msrdir = msrdir,
      force = force,
      verbose = FALSE
    )
    return(invisible(saved))

  } else {
    df <- load_aux(
      maindir = maindir,
      measure = measure,
      branch  = branch
    )
    return(df)
  }
}

#' Validate raw maddison data
#'
#' @param mpd raw mpd data, as loaded via `pipfun::load_from_gh`
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#' @keywords internal
#'
#' @export
mpd_validate_raw <- function(mpd, detail = getOption("pipaux.detail.raw")){

  stopifnot("mpd/ maddison raw data is not loaded" = !is.null(mpd))

  report <- data_validation_report()

  validate(mpd, name = "mdp raw data validation") |>
    validate_if(is.character(country_code),
                description = "`country_code` should be character") |>
    validate_if(is.numeric(year),
                description = "`year` should be numeric") |>
    validate_if(is.numeric(mpd_gdp),
                description = "`mpd_gdp` should be numeric") |>
    validate_cols(not_na, country_code, year,
                  description = "no missing values in key variables") |>
    validate_if(is_uniq(country_code, year),
                description = "no duplicate records in key variables") |>
    add_results(report)

  validation_record <- get_results(report, unnest = FALSE) |>
    setDT()

  if (any(validation_record[["type"]] == "error")){
    get_error_validation(validation_record, detail)
  }

}

