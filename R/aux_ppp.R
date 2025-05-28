#' PIP PPP
#'
#' Load or update PPP data.
#'
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @inheritParams aux_pfw
#' @inheritParams pipfun::load_from_gh
#' @export
#' @import data.table
aux_ppp <- function(action = c("update", "load"),
                    maindir = gls$PIP_DATA_DIR,
                    owner   = getOption("pipfun.ghowner"),
                    force   = FALSE,
                    tag     = NULL,
                    detail  = getOption("pipaux.detail.raw"),
                    ppp_defaults = TRUE) {

  pipfun::get_wrk_release(verbose = FALSE)

  release        <- wrk_release$release
  identity       <- wrk_release$identity
  branch         <- paste0(release, "_", identity)

  if (is.null(tag)) {
    tag <- paste0(release, "_", identity)
  }

  #   ____________________________________________________________________________
  #   on.exit                                                                 ####
  on.exit({

  })

  #   ____________________________________________________________________________
  #   Defenses                                                                ####
  measure <- "ppp"
  action <- match.arg(action)


  stopifnot( exprs = {

  }
  )

  #   ____________________________________________________________________________
  #   Early returns                                                           ####
  if (FALSE) {
    return()
  }

  #   ____________________________________________________________________________
  #   Computations                                                            ####
  if (action == "update") {
    aux_ppp_update(maindir = maindir,
                   force   = force,
                   owner   = owner,
                   branch  = branch,
                   tag     = tag,
                   detail  = detail)
  }
  else {
    load_aux(
      maindir = maindir,
      measure = measure,
      branch  = branch,
      ppp_defaults = ppp_defaults
    )
  }


}

#' Clean PPP data from datalibweb to meet PIP protocols
#'
#' @param y dataset with PPP data from `aux_ppp_update()`.
#' @param default_year numeric: ICP round year. Default is 2011
#'
#' @keywords internal
aux_ppp_clean <- function(y, default_year = getOption("pipaux.pppyear")) {
  x <- data.table::as.data.table(y)

  y <- melt(x,
            id.vars       = c("code", "ppp_domain", "datalevel"),
            measure.vars  = patterns("^ppp_[0-9]{4}_[Vv][0-9]_[Vv][0-9]$"),
            variable.name = "ver",
            value.name    = "ppp"
  )

  y[
    ,
    c("p", "ppp_year", "release_version", "adaptation_version") := tstrsplit(ver, "_")
  ][
    ,
    `:=`(
      ppp_year   = as.numeric(ppp_year),
      ppp_domain = as.character(ppp_domain),
      datalevel  = as.character(datalevel)
    )
  ][
    ,
    # This part should not exist if the raw data
    # has been properly created
    ppp_data_level := fcase(
      ppp_domain %chin% c("urban/rural", "2") & datalevel == "0", "rural",
      ppp_domain %chin% c("urban/rural", "2") & datalevel == "1", "urban",
      ppp_domain %chin% c("national", "1") & datalevel %chin% c("2", "", NA_character_), "national",
      default = ""
    )
  ][
    ,
    c("p", "ver", "datalevel") := NULL
  ]

  setorder(y, code, ppp_year, release_version, adaptation_version)

  #--------- Get default version ---------

  y[ # Find Max release version
    ,
    d1 := release_version == max(release_version),
    by = .(code, ppp_year)
  ][
    # Find max adaptation version of the max release
    d1 == TRUE,
    d2 := adaptation_version == max(adaptation_version),
    by = .(code, ppp_year)
  ][
    ,
    # get intersection
    `:=`(
      ppp_default         = (d1 == TRUE & d2 == TRUE & ppp_year == (default_year)),
      ppp_default_by_year = (d1 == TRUE & d2 == TRUE),
      country_code        = code
    )
  ][
    ,
    # Remove unnecessary variables
    c("d1", "d2", "code") := NULL
  ]

  setcolorder(
    y,
    c(
      "country_code",
      "ppp_year",
      "release_version",
      "adaptation_version",
      "ppp",
      "ppp_default",
      "ppp_default_by_year",
      "ppp_domain",
      "ppp_data_level"
    )
  )

  y <- unique(y) # remove duplicates

  # Remove non WDI countries
  non_wdi <- c("BES", "EGZ", "RUT", "SDO")
  if (any(y$country_code %in% non_wdi)) {
    y <- y[!(country_code %in% non_wdi)]
  }

  return(y)
}

#' Update PPP
#'
#' @inheritParams pipfun::load_from_gh
#' @keywords internal
aux_ppp_update <- function(maindir = gls$PIP_DATA_DIR,
                           force   = FALSE,
                           owner   = getOption("pipfun.ghowner"),
                           branch  = paste0(wrk_release$release, "_", wrk_release$identity),
                           tag     = match.arg(branch),
                           detail  = getOption("pipaux.detail.raw")) {


  #   ____________________________________________________________________________
  #   set up                                                                  ####

  measure <- "ppp"


  #   ____________________________________________________________________________
  #   Load raw data                                                           ####

  ppp <- pipfun::load_from_gh(
    measure = measure,
    owner  = owner,
    branch = branch,
    tag    = tag,
    ext    = "csv"
  )

  gh <- attr(ppp, "gh")

  # DEBUG
  #print(attr(ppp, "gh"))

  # validate ppp raw data
  ppp_validate_raw(ppp = ppp, detail = detail)

  #   ____________________________________________________________________________
  #   cleaning                                                                ####

  # Clean data
  ppp <- aux_ppp_clean(ppp)



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

  # drop ppp_domain
  ppp <- ppp[, -c("ppp_domain")]

  ppp <- ppp |> setnames("ppp_data_level", "reporting_level",
                         skip_absent=TRUE)

  setattr(ppp, "aux_name", "ppp")

  setattr(ppp,
          "aux_key",
          c("country_code", "reporting_level")) # this is going to be key variables only when PPP default year selected.

  setattr(ppp, "gh", gh)

  # validate ppp output data
  ppp_validate_output(ppp = ppp, detail = detail)

  if (branch == "main") {
    branch <- ""
  }

  # ----- function raw sha ------
  raw_sha_fun <- digest::digest(body(
    paste0("aux_", measure))
  )
  setattr(ppp,
          "raw_sha_fun",
          raw_sha_fun)


  msrdir <- fs::path(maindir, "aux_data", branch, measure) # measure dir

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

  # ppp_vintage <- ppp_vintage |> setnames("ppp_data_level", "reporting_level",
  #                        skip_absent=TRUE)
  #
  # setattr(ppp_vintage, "aux_name", "ppp")
  # setattr(ppp_vintage,
  #         "aux_key",
  #         c("country_code", "reporting_level"))

  # Save
  pipfun::pip_sign_save(
    x = ppp_vintage,
    measure = "ppp_vintage",
    msrdir = msrdir,
    force = force
  )

  return(invisible(saved))
}

#' Validate output ppp data
#'
#' @param ppp output ppp data
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#' @keywords internal
#'
#' @export
ppp_validate_output <- function(ppp, detail = getOption("pipaux.detail.output")){

  stopifnot("PPP output data is not loaded" = !is.null(ppp))

  report <- data_validation_report()

  validate(ppp, name = "PPP output data validation") |>
    validate_if(is.character(country_code),
                description = "`country_code` should be character") |>
    validate_if(is.numeric(ppp_year),
                description = "`ppp_year` should be character") |>
    validate_if(is.character(release_version),
                description = "`release_version` should be character") |>
    validate_if(is.character(adaptation_version),
                description = "`adaptation_version` should be character") |>
    validate_if(is.numeric(ppp),
                description = "`ppp` should be numeric") |>
    validate_if(is.logical(ppp_default),
                description = "`ppp_default` should be numeric") |>
    validate_if(is.logical(ppp_default_by_year),
                description = "`ppp_default_by_year` should be numeric") |>
    # validate_if(is.character(ppp_domain),
    #             description = "`ppp_domain` should be character") |>
    # validate_cols(in_set(c("1", "2")),
    #               ppp_domain, description = "`ppp_domain` values within range") |>
    validate_if(is.character(reporting_level),
                description = "`reporting_level` should be character") |>
    validate_cols(in_set(c("national", "rural", "urban")),
                  reporting_level, description = "`reporting_level` values within range") |>
    validate_cols(not_na, country_code, ppp_year, reporting_level,
                  adaptation_version, release_version,
                  description = "no missing values in key variables") |>
    validate_if(is_uniq(country_code, ppp_year,
                        reporting_level, adaptation_version, release_version),
                description = "no duplicate records in key variables") |>
    add_results(report)

  validation_record <- get_results(report, unnest = FALSE) |>
    setDT()

  if (any(validation_record[["type"]] == "error")){
    get_error_validation(validation_record, detail)
  }

}

#' Validate raw ppp data
#'
#' @param ppp raw ppp data, as loaded via `pipfun::load_from_gh`
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#' @keywords internal
#'
#' @export
ppp_validate_raw <- function(ppp, detail = getOption("pipaux.detail.raw")){

  stopifnot("PPP raw data is not loaded" = !is.null(ppp))

  report <- data_validation_report()

  validate(ppp, name = "PPP raw data validation") |>
    validate_if(is.character(CountryName),
                description = "`CountryName` should be character") |>
    validate_if(is.character(code),
                description = "`code` should be character") |>
    validate_if(is.character(CoverageType),
                description = "`CoverageType` should be character") |>
    validate_cols(in_set(c("National", "Rural", "Urban")),
                  CoverageType, description = "`CoverageType` values within range") |>
    validate_if(is.numeric(ppp_2005_v1_v1),
                description = "`ppp_2005_v1_v1` should be numeric") |>
    validate_if(is.numeric(ppp_2011_v1_v1),
                description = "`ppp_2011_v1_v1` should be numeric") |>
    validate_if(is.numeric(ppp_2011_v2_v1),
                description = "`ppp_2011_v2_v1` should be numeric") |>
    validate_if(is.numeric(ppp_2011_v1_v2),
                description = "`ppp_2011_v1_v2` should be numeric") |>
    validate_if(is.numeric(ppp_2011_v2_v2),
                description = "`ppp_2011_v2_v2` should be numeric") |>
    validate_if(is.numeric(ppp_2017_v1_v1),
                description = "`ppp_2017_v1_v1` should be numeric") |>
    validate_if(is.numeric(ppp_2017_v1_v2),
                description = "`ppp_2017_v1_v2` should be numeric") |>
    validate_if(is.numeric(source_ppp_2011),
                description = "`source_ppp_2011` should be numeric") |>
    validate_if(is.numeric(source_ppp_2005),
                description = "`source_ppp_2005` should be numeric") |>
    validate_if(is.numeric(datalevel),
                description = "`datalevel` should be numeric") |>
    validate_cols(in_set(c(0, 1, 2)),
                  datalevel, description = "`datalevel` values within range") |>
    validate_if(is.numeric(ppp_domain),
                description = "`ppp_domain` should be numeric") |>
    validate_cols(in_set(c(1, 2)),
                  ppp_domain, description = "`ppp_domain` values within range") |>
    validate_if(is.numeric(ppp_domain_value),
                description = "`ppp_domain_value` should be numeric") |>
    validate_cols(in_set(c(1, 2)),
                  ppp_domain_value, description = "`ppp_domain_value` values within range") |>
    validate_if(is.numeric(oldicp2005),
                description = "`oldicp2005` should be numeric") |>
    validate_if(is.numeric(oldicp2011),
                description = "`oldicp2011` should be numeric") |>
    validate_if(is.character(Seriesname),
                description = "`Seriesname` should be character") |>
    validate_if(is.character(note_may192020),
                description = "`note_may192020` should be character") |>
    validate_if(is.character(ppp_2017_v1_v2_note),
                description = "`ppp_2017_v1_v2_note` should be character") |>
    validate_cols(not_na, code, CoverageType, datalevel,
                  description = "no missing values in key variables") |>
    validate_if(is_uniq(code, CoverageType, datalevel),
                description = "no duplicate records in key variables") |>
    add_results(report)

  validation_record <- get_results(report, unnest = FALSE) |>
    setDT()

  if (any(validation_record[["type"]] == "error")){
    get_error_validation(validation_record, detail)
  }

}
