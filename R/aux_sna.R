#' PIP Special National accounts
#'
#' Update special national accounts data
#'
#' @inheritParams aux_pfw
#' @inheritParams pipfun::load_from_gh
#' @param from character: Either "gh", "file" or "api". Default is "gh". "file"
#'   and "gh" are synonymous
#' @export
aux_sna <- function(action          = c("update", "load"),
                    force           = FALSE,
                    maindir         = gls$PIP_DATA_DIR,
                    owner           = getOption("pipfun.ghowner"),
                    branch          = c("DEV", "PROD", "main"),
                    tag             = match.arg(branch)) {

  measure    <- "sna"
  branch <- match.arg(branch)
  action <- match.arg(action)


  if (action == "update") {
    # load nowcast growth rates
    sna <- pipfun::load_from_gh(
      measure = "sna",
      owner  = owner,
      branch = branch
    )
    if (branch == "main") {
      branch <- ""
    }
    msrdir <- fs::path(maindir, "_aux", branch, measure) # measure dir

    saved <- pipfun::pip_sign_save(
      x       = sna,
      measure = measure,
      msrdir  = msrdir,
      force   = force
    )

  } else {
    dt <- load_aux(
      maindir = maindir,
      measure = measure,
      branch  = branch
    )
    return(dt)
  }
} # end

#' Validate raw special national accounts (sna) data
#'
#' @param sna raw sna data, as loaded via `pipfun::load_from_gh`
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#' @keywords internal
#'
#' @export
sna_validate_raw <- function(sna, detail = getOption("pipaux.detail.raw")){

  stopifnot("SNA raw data is not loaded" = !is.null(sna))

  report <- data_validation_report()

  validate(sna, name = "SNA raw data validation") |>
    validate_if(is.character(countryname),
                description = "`countryname` should be character") |>
    validate_if(is.character(coverage),
                description = "`coverage` should be character") |>
    validate_cols(in_set(c("National")),
                  coverage, description = "`coverage` values within range") |>
    validate_if(is.character(countrycode),
                description = "`countrycode` should be character") |>
    validate_if(is.numeric(year),
                description = "`year` should be numeric") |>
    validate_if(is.numeric(GDP),
                description = "`GDP` should be numeric") |>
    validate_if(is.logical(PCE),
                description = "`PCE` should be logical") |>
    validate_if(is.character(sourceGDP),
                description = "`sourceGDP` should be character") |>
    validate_if(is.logical(sourcePCE),
                description = "`sourcePCE` should be logical") |>
    validate_cols(not_na, countrycode, year,
                  description = "no missing values in key variables") |>
    validate_if(is_uniq(countrycode, year),
                description = "no duplicate records in key variables") |>
    add_results(report)

  validation_record <- get_results(report, unnest = FALSE) |>
    setDT()

  if (any(validation_record[["type"]] == "error")){
    get_error_validation(validation_record, detail)
  }

}

#' Validate raw sna_fy data
#'
#' @param sna_fy raw sna_fy data, as loaded via `pipfun::load_from_gh`
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#' @keywords internal
#'
#' @export
sna_fy_validate_raw <- function(sna_fy, detail = getOption("pipaux.detail.raw")){

  stopifnot("sna_fy raw data is not loaded" = !is.null(sna_fy))

  report <- data_validation_report()

  validate(sna_fy, name = "sna_fy raw data validation") |>
    validate_if(is.character(Code),
                description = "`Code` should be character") |>
    validate_if(is.character(LongName),
                description = "`LongName` should be character") |>
    validate_if(is.character(SpecialNotes),
                description = "`SpecialNotes` should be character") |>
    validate_if(is.character(Month),
                description = "`Month` should be character") |>
    validate_if(is.numeric(Day),
                description = "`Day` should be numeric") |>
    validate_cols(not_na, Code, Month, Day,
                  description = "no missing values in key variables") |>
    # validate_if(is_uniq(Code, LongName),
    #             description = "no duplicate records in key variables") |>
    add_results(report)

  validation_record <- get_results(report, unnest = FALSE) |>
    setDT()

  if (any(validation_record[["type"]] == "error")){
    get_error_validation(validation_record, detail)
  }

}

#' Fake PIP SNA function
#'
#' @inheritParams aux_gdp
#' @inheritParams aux_pfw
#' @inheritParams pipfun::load_from_gh
#' @export
fake_aux_sna <- function(action  = c("update", "load"),
                         force   = FALSE,
                         owner   = getOption("pipfun.ghowner"),
                         maindir = gls$PIP_DATA_DIR,
                         branch  = c("DEV", "PROD", "main"),
                         tag     = match.arg(branch),
                         from    = c("gh", "file", "api")) {

  return(invisible(TRUE))
}



