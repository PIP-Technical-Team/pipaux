#' National Poverty headcount
#'
#' Update series of national poverty lines
#'
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @inheritParams aux_cpi
#' @inheritParams pipfun::load_from_gh
#' @export
aux_npl <- function(action  = c("update", "load"),
                   force   = FALSE,
                   owner   = getOption("pipfun.ghowner"),
                   maindir = gls$PIP_DATA_DIR,
                   tag     = match.arg(branch),
                   detail  = getOption("pipaux.detail.raw")) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## setup --------

  measure <- "npl"
  action <- match.arg(action)

  pipfun::get_wrk_release(verbose = FALSE)

  release        <- wrk_release$release
  identity       <- wrk_release$identity
  branch         <- paste0(release, "_", identity)

  if (action == "update") {
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## update --------

    npl <- pipfun::load_from_gh(measure = measure,
                                owner  = owner,
                                branch = branch,
                                tag    = tag,
                                ext    = "dta") |>
      setDT()

    # validate npl raw data
    npl_validate_raw(npl = npl, detail = detail)

    setnames(x = npl,
             old = c("countrycode",  "year", "vsi_pov_nahc_nc"),
             new = c("country_code", "reporting_year", "nat_headcount"),
             skip_absent = TRUE)

    npl[, c("region", "vsi_pov_nahc") := NULL]
    npl[, nat_headcount := nat_headcount / 100]


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## save --------
    npl <- npl |> setnames("reporting_year", "year",
                           skip_absent=TRUE)

    setattr(npl, "aux_name", "npl")
    setattr(npl,
            "aux_key",
            c("country_code", "year"))

    # validate npl output data
    npl_validate_output(npl = npl, detail = detail)

    if (branch == "main") {
      branch <- ""
    }
    msrdir <- fs::path(maindir, "aux_data", branch, measure) # measure dir

    # ----- function raw sha ----------------------

    raw_sha_fun <- digest::digest(body(
      paste0("aux_", measure))
    )


    setattr(npl,
            "raw_sha_fun",
            raw_sha_fun)

    saved <- pipfun::pip_sign_save(
      x       = npl,
      measure = measure,
      msrdir  = msrdir,
      force   = force
    )


  } else {
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## load --------

    load_aux(
      maindir = maindir,
      measure = measure,
      branch  = branch
    )

  }
}

#' Validate npl raw data
#'
#' @param npl raw npl data, as loaded via `pipfun::load_from_gh`
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#' @keywords internal
#'
#' @export
npl_validate_raw <- function(npl, detail = getOption("pipaux.detail.raw")){

  stopifnot("NPL raw data is not loaded" = !is.null(npl))

  report <- data_validation_report()

  validate(npl, name = "NPL raw data validation") |>
    validate_if(is.character(region),
                description = "`region` should be character") |>
    # validate_cols(in_set(c("AFE", "AFW", "EAP", "ECA", "LAC", "MNA", "SAR")),
    #               region, description = "`region` values within range") |>
    validate_if(is.character(countrycode),
                description = "`countrycode` should be character") |>
    validate_if(is.numeric(year),
                description = "`year` should be numeric") |>
    validate_if(is.numeric(vsi_pov_nahc_nc),
                description = "`vsi_pov_nahc_nc` should be numeric") |>
    validate_if(is.numeric(vsi_pov_nahc),
                description = "`vsi_pov_nahc` should be numeric") |>
    validate_if(is.numeric(comparability),
                description = "`comparability` should be numeric") |>
    validate_if(is.character(footnote),
                description = "`footnote` should be character") |>
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

#' Validate npl output data
#'
#' @param npl output data
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#' @keywords internal
#'
#' @export
npl_validate_output <- function(npl, detail = getOption("pipaux.detail.output")){

  stopifnot("NPL output data is not loaded" = !is.null(npl))

  report <- data_validation_report()

  validate(npl, name = "NPL output data validation") |>
    validate_if(is.character(country_code),
                description = "`country_code` should be character") |>
    validate_if(is.numeric(year),
                description = "`year` should be numeric") |>
    validate_if(is.numeric(nat_headcount),
                description = "`nat_headcount` should be numeric") |>
    validate_if(is.numeric(comparability),
                description = "`comparability` should be numeric") |>
    validate_if(is.character(footnote),
                description = "`footnote` should be character") |>
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
