#' PIP POP
#'
#' Load or update population data.
#'
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @inheritParams aux_cpi
#' @inheritParams pipfun::load_from_gh
#' @param from character: Source for population data.
#' @export
aux_pop <- function(action = c("update", "load"),
                    owner   = getOption("pipfun.ghowner"),
                    tag     = NULL,
                    detail  = getOption("pipaux.detail.raw")) {
  measure <- "pop"
  action <- match.arg(action)

  wrk_release <- get_from_auxenv(key = "wrk_release")

  release        <- wrk_release$release
  identity       <- wrk_release$identity
  branch         <- paste0(release, "_", identity)

  if (is.null(tag)) {
    tag <- paste0(release, "_", identity)
  }

  if (action == "update") {
    aux_pop_update(
      owner   = owner,
      branch  = branch,
      tag     = tag,
      detail  = detail)

  } else {

    df <- pipload::load_aux_data(measure = measure)

    return(df)
  }
}

#' Update POP
#'
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @param from character: Source for population data.
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @inheritParams aux_pop
aux_pop_update <-  function(owner   = getOption("pipfun.ghowner"),
                            branch  = NULL,
                            tag     = branch,
                            detail  = getOption("pipaux.detail.raw")) {


  tag     <- branch
  measure <- "pop"

  # Get current year as max year

  year_max <- Sys.Date() |>
    format("%Y") |>
    as.numeric()
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # From WDI   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## from Emi's file --------

    # Now Emi's file is uploaded directly to GH. So we get it from there.
    # Load data

    pop_main <- pipfun::load_from_gh(
      measure = measure,
      owner  = owner,
      branch = branch,
      tag    = tag,
      ext    = "xlsx"
    )

    ### Get the attributes before they get lost
    gh_pop_main <- attr(pop_main, "gh")

    pop_main <- pop_main |>
      clean_names_from_wide() |>
      clean_from_wide()

    # validate pop main raw data
    popmain_validate_raw(pop_main = pop_main, detail = detail)

    ### Ger special cases ---------
    spop <- pipfun::load_from_gh(
      measure = measure,
      filename = "spop",
      owner  = owner,
      branch = branch,
      tag    = tag,
      ext    = "csv"
    )

    ### Get the attributes before they get lost
    gh_spop <- attr(spop, "gh")

    spop <- spop |>
      clean_names_from_wide() |>
      clean_from_wide()

    # validate special cases pop raw data
    spop_validate_raw(spop = spop, detail = detail)

    pop <- joyn::joyn(pop_main, spop,
                      by = c("country_code", "year", "pop_data_level"),
                      update_values = TRUE,
                      reportvar = FALSE,
                      verbose = FALSE)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Clean data   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Remove years prior to 1960
  pop <- pop[!is.na(pop) & year >= 1960]
  pop <- pop[year <= year_max]

  # sorting
  setorder(pop, country_code, year, pop_data_level)
  setcolorder(pop, c("country_code", "year", "pop_data_level", "pop"))

  pop[,
      pop_domain := fifelse(pop_data_level == 2, 1, 2)]

  # recode domain and data_level variables
  cols <- c("pop_domain", "pop_data_level")
  pop[,
      (cols) := lapply(.SD, as.character),
      .SDcols = cols
  ][
    , # recode domain
    pop_domain := fcase(
      pop_domain == "1", "national",
      pop_domain == "2", "urban/rural",
      pop_domain == "3", "subnational region"
    )
  ][ # Recode data_level only for those that are national or urban/rural
    pop_domain %in% c("national", "urban/rural"),
    pop_data_level := fcase(
      pop_data_level == "0", "rural",
      pop_data_level == "1", "urban",
      pop_data_level == "2", "national"
    )
  ]


  # Remove any non-WDI countries
  cl <- pipload::load_aux_data(measure = "country_list")

  setDT(cl)
  pop <- pop[country_code %in% cl$country_code] |>
    unique() # make sure we don't havce any duplicates

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Save data   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # drop pce_domain
  pop <- pop[, -c("pop_domain")]

  pop <- pop |> setnames("pop_data_level", "reporting_level",
                         skip_absent=TRUE)

  setattr(pop, "aux_name", "pop")

  # validate output pop data
  pop_validate_output(pop = pop, detail = detail)

  # Save
  if (branch == "main") {
    branch <- ""
  }

  # Set gh attributes --------------------------------

  setattr(pop,
          "gh",
          list(gh_spop = gh_spop,
               gh_pop_main = gh_pop_main))


  key_cols <- c("country_code", "year", "reporting_level")
  setattr(pop, "aux_key", key_cols)
  
  saved <- pip_aux_save(
    x        = pop,
    id       = measure,
    pk       = key_cols,
    metadata = list(gh = list(gh_spop = gh_spop, gh_pop_main = gh_pop_main)),
    code     = aux_pop_update,
    code_label = "aux_pop_update"
  )


  return(invisible(saved))

}



#' Clean names from wide WDI format
#'
#' @param x data frame
#'
#' @return dataframe with names cleaned
#' @keywords internal
clean_names_from_wide <- function(x) {
  if (!is.data.table(x)) {
    setDT(x)
  }
  nnames <- as.character(x[2, 1:4])
  setnames(x, 1:4, nnames)
  x <- x[-c(1:2)]
  x
}


#' Clean from WDI format
#'
#' @param x data frame
#'
#' @return dataframe with names cleaned
#' @keywords internal
clean_from_wide <- function(x) {
  if (!is.data.table(x)) {
    setDT(x)
  }


  year_vars            <- names(x)[6:ncol(x)]
  x$Series_Name <- NULL
  x$Time_Name   <- NULL

  # Reshape to long format
  pop_long <- x |>
    data.table::setDT() |>
    data.table::melt(
      id.vars = c("Country", "Series"),
      measure.vars = year_vars,
      variable.name = "Year",
      value.name = "Population"
    )
  pop_long[,
           Year := as.numeric(as.character(Year))
  ][,
    Population := {
      Population[Population == "."] <- NA_character_
      as.numeric(Population)
    }]



  pop <- pop_long
  # Create data_level column
  pop[,
      pop_data_level :=
        fcase(
          grepl("POP", Series), 2,
          grepl("RUR", Series), 0,
          grepl("URB", Series), 1
        )
  ][,
    Series := NULL]

  # Set colnames
  setnames(
    pop,
    old = c("Country", "Year", "Population"),
    new = c("country_code", "year", "pop")
  )

  return(pop)
}

#' Validate raw main pop data
#'
#' @param pop_main raw pop main data, as loaded via `pipfun::load_from_gh`
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#' @keywords internal
#'
#' @export
popmain_validate_raw <- function(pop_main, detail = getOption("pipaux.detail.raw")){

  stopifnot("POP main raw data is not loaded" = !is.null(pop_main))

  report <- data_validation_report()

  validate(pop_main, name = "POP main raw data validation") |>
    validate_if(is.character(country_code),
                description = "`country_code` should be character") |>
    validate_if(is.numeric(year),
                description = "`year` should be numeric") |>
    validate_if(is.numeric(pop_data_level),
                description = "`pop_data_level` should be numeric") |>
    validate_cols(in_set(c(0, 1, 2)),
                  pop_data_level, description = "`pop_data_level` values within range") |>
    validate_if(is.numeric(pop),
                description = "`pop` should be numeric") |>
    validate_cols(not_na, country_code, year, pop_data_level,
                  description = "no missing values in key variables") |>
    validate_if(is_uniq(country_code, year, pop_data_level),
                description = "no duplicate records in key variables") |>
    add_results(report)

  validation_record <- get_results(report, unnest = FALSE) |>
    setDT()

  if (any(validation_record[["type"]] == "error")){
    get_error_validation(validation_record, detail)
  }

}

#' Validate pop raw data download from wdi
#'
#' @param pop raw pop data, as loaded via `wbstats::wb_data`
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#' @keywords internal
#'
#' @export
pop_validate_raw <- function(pop, detail = getOption("pipaux.detail.output")){

  stopifnot("WB POP raw data is not loaded" = !is.null(pop))

  report <- data_validation_report()

  validate(pop, name = "WB POP raw data validation") |>
    validate_if(is.character(indicator_id),
                description = "`indicator_id` should be character") |>
    validate_cols(in_set(c("SP.POP.TOTL", "SP.RUR.TOTL", "SP.URB.TOTL")),
                  indicator_id, description = "`indicator_id` values within range") |>
    validate_if(is.character(indicator),
                description = "`indicator` should be character") |>
    validate_if(is.character(iso2c),
                description = "`iso2c` should be character") |>
    validate_if(is.character(iso3c),
                description = "`iso3c` should be character") |>
    validate_if(is.character(country),
                description = "`country` should be character") |>
    validate_if(is.numeric(date),
                description = "`date` should be numeric") |>
    validate_if(is.numeric(value),
                description = "`value` should be numeric") |>
    validate_if(is.character(unit),
                description = "`unit` should be character") |>
    validate_if(is.character(obs_status),
                description = "`obs_status` should be character") |>
    validate_if(is.character(footnote),
                description = "`footnote` should be character") |>
    validate_if(is_date(last_updated),
                description = "`last_updated` should be date") |>
    validate_cols(not_na, indicator_id, iso3c, date,
                  description = "no missing values in key variables") |>
    validate_if(is_uniq(indicator_id, iso3c, date),
                description = "no duplicate records in key variables") |>
    add_results(report)

  validation_record <- get_results(report, unnest = FALSE) |>
    setDT()

  if (any(validation_record[["type"]] == "error")){
    get_error_validation(validation_record, detail)
  }

}

#' Validate output pop data
#'
#' @param pop output pop data
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#' @keywords internal
#'
#' @export
pop_validate_output <- function(pop, detail = getOption("pipaux.detail.output")){

  stopifnot("POP clean data is not loaded" = !is.null(pop))

  report <- data_validation_report()

  validate(pop, name = "POP output data validation") |>
    validate_if(is.character(country_code),
                description = "`country_code` should be character") |>
    validate_if(is.numeric(year),
                description = "`year` should be numeric") |>
    validate_if(is.character(reporting_level),
                description = "`reporting_level` should be character") |>
    validate_cols(in_set(c("national", "rural", "urban")),
                  reporting_level, description = "`reporting_level` values within range") |>
    validate_if(is.numeric(pop),
                description = "`pop` should be numeric") |>
    # validate_if(is.character(pop_domain),
    #             description = "`pop_domain` should be character") |>
    # validate_cols(in_set(c("national", "urban/rural")),
    #               pop_domain, description = "`pop_domain` values within range") |>
    validate_cols(not_na, country_code, year, reporting_level,
                  description = "no missing values in key variables") |>
    validate_if(is_uniq(country_code, year, reporting_level),
                description = "no duplicate records in key variables") |>
    add_results(report)

  validation_record <- get_results(report, unnest = FALSE) |>
    setDT()

  if (any(validation_record[["type"]] == "error")){
    get_error_validation(validation_record, detail)
  }

}

#' Validate raw special cases pop data
#'
#' @param spop raw special case pop data, as loaded via `pipfun::load_from_gh`
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#' @keywords internal
#'
#' @export
spop_validate_raw <- function(spop, detail = getOption("pipaux.detail.output")){

  stopifnot("Special POP raw data is not loaded" = !is.null(spop))

  report <- data_validation_report()

  validate(spop, name = "Special POP raw data validation") |>
    validate_if(is.character(country_code),
                description = "`country_code` should be character") |>
    validate_if(is.numeric(year),
                description = "`year` should be numeric") |>
    validate_if(is.numeric(pop_data_level),
                description = "`pop_data_level` should be numeric") |>
    validate_cols(in_set(c(0, 1, 2)),
                  pop_data_level, description = "`pop_data_level` values within range") |>
    validate_if(is.numeric(pop),
                description = "`pop` should be numeric") |>
    validate_cols(not_na, country_code, year, pop_data_level,
                  description = "no missing values in key variables") |>
    validate_if(is_uniq(country_code, year, pop_data_level),
                description = "no duplicate records in key variables") |>
    add_results(report)

  validation_record <- get_results(report, unnest = FALSE) |>
    setDT()

  if (any(validation_record[["type"]] == "error")){
    get_error_validation(validation_record, detail)
  }

}


