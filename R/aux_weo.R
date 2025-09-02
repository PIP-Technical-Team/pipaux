#' Fetch GDP data from WEO
#'
#' Create a dataset with GDP data from World Economic Outlook.
#'
#' Note that the most recent version most be downloaded from imf.org and saved
#' as an .xls file in `<maindir>/_aux/weo/`. The filename should be in the
#' following structure `WEO_<YYYY-DD-MM>.xls`. Due to potential file corruption
#' the file must be opened and re-saved before it can be updated with
#' `aux_weo()`. Hopefully in the future IMF will stop using an `.xls` file
#' that's not really xls.
#'
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @inheritParams aux_pfw
#' @inheritParams pipfun::load_from_gh
#' @export
aux_weo <- function(action  = c("update", "load"),
                    force   = FALSE,
                    owner   = getOption("pipfun.ghowner"),
                    tag     = NULL,
                    detail  = getOption("pipaux.detail.raw")) {

  measure <- "weo"
  action <- match.arg(action)

  wrk_release <- get_from_auxenv(key = "wrk_release")

  release        <- wrk_release$release
  identity       <- wrk_release$identity
  branch         <- paste0(release, "_", identity)

  if (is.null(tag)) {
    tag <- paste0(release, "_", identity)
  }

  if (action == "update") {

    # ---- Load data from disk ----

    # Read data
    dt <- pipfun::load_from_gh(
      measure = measure,
      owner  = owner,
      branch = branch,
      tag    = tag,
      ext    = "csv"
    )

    # Save raw attributes before they get lost when cleaning
    gh <- attr(dt, "gh")

    # validate weo raw data
    weo_validate_raw(weo = dt, detail = detail)

    dt <- aux_weo_clean(dt,
                        branch = branch)

    # Save dataset
    setattr(dt, "aux_name", "weo")
    setattr(dt,
            "aux_key",
            c("country_code", "year"))

    setattr(dt, "gh", gh)

    # validate weo clean data
    weo_validate_output(weo = dt, detail = detail)

    # ----- function raw sha -----------------------------
    raw_sha_fun <- digest::digest(body(
      paste0("aux_", measure))
    )

    setattr(dt,
            "raw_sha_fun",
            raw_sha_fun)

    if (branch == "main") {
      branch <- ""
    }

    saved <- pip_aux_save(
      x        = dt,
      pin_name = measure,
      force    = force
    )

    return(invisible(saved))

  } else {

    dt <- pipload::load_aux_data(measure = measure)

    return(dt)
  }
}

#' Clean WEO data
#'
#' @param dt database with weo raw data
#' @param maindir directory where auxiliary data is stored (to load pop)
#' @param branch character: branch to be loaded
#'
#' @return data.table
#' @export
aux_weo_clean <- function(dt,
                          branch  = NULL) {

  #   _________________________________________
  #   Computations                        ####
  if (!inherits(dt, "data.table")) {
    setDT(dt)
  }

  # Clean column names
  nn <-
    names(dt) |>
    tolower() |>
    {\(.) gsub("[-/ ]", "_", .)}() |>
    {\(.) gsub("([0-9]{4})", "x\\1", .)}()

  names(dt) <- nn

  # ---- Data transformations ----

  # Select rows w/ data on real gdp per capita
  dt <- dt[weo_subject_code %in% c("NGDPRPC", "NGDPRPPPPC")]

  # Fix country codes
  dt[
    ,
    iso := fifelse(
      iso == "WBG", "PSE", iso # West Bank & Gaza
    )
  ][
    ,
    iso := fifelse(
      iso == "UVK", "XKX", iso # Kosovo
    )
  ][,
    # Replace subject codes
    subject_code := fcase(
      weo_subject_code == "NGDPRPC", "weo_gdp_lcu",
      weo_subject_code == "NGDPRPPPPC", "weo_gdp_ppp2017"
    )
  ]

  # Reshape to long format

  years_vars <- names(dt)[grepl("\\d{4}", names(dt))]
  dt <-
    melt(data = dt,
         id.vars = c("iso", "subject_code"),
         measure.vars = years_vars,
         value.name = "weo_gdp",
         variable.name = "year"
    )
  setnames(dt, "iso", "country_code")

  # Convert year and GDP to numeric
  dt[,
     c("weo_gdp", "year") := {
       y <- sub("x", "", year) |>
         as.numeric()

       x <- as.numeric(weo_gdp) |>
         suppressWarnings()
       list(x, y)
     }]

  # Remove rows w/ missing GDP`
  dt <- na.omit(dt, cols = "weo_gdp")

  # Remove current year and future years
  current_year <- format(Sys.Date(), "%Y")
  dt <- dt[year < current_year]

  # Reshape to wide for GDP columns
  dt <- dcast(dt,
              formula = country_code + year ~ subject_code,
              value.var = "weo_gdp"
  )

  # ---- Merge with population ----


  pop <- pipload::load_aux_data(measure = "pop")

  setDT(pop)
  pop <- pop[reporting_level == "national", ] #pop_data_level = reporting_level
  dt[pop,
     on = .(country_code, year),
     `:=`(
       pop = i.pop
     )
  ]

  # ---- Chain PPP and LCU GDP columns ----

  # Chain LCU on PPP column

  dt[, weo_gdp := chain_val(ori_var = weo_gdp_ppp2017,
                            rep_var = weo_gdp_lcu),
     by = country_code]

  # --- Sign and save ----

  # Select final columns
  dt <- dt[, c("country_code", "year", "weo_gdp")]



  #   ____________________________________________________________________________
  #   Return                                                                  ####
  return(dt)

}

#' Validate clean weo data
#'
#' @param weo clean weo data
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#' @keywords internal
#'
#' @export
weo_validate_output <- function(weo, detail = getOption("pipaux.detail.output")){

  stopifnot("WEO output data is not loaded" = !is.null(weo))

  report <- data_validation_report()

  validate(weo, name = "WEO output data validation") |>
    validate_if(is.character(country_code),
                description = "`country_code` should be character") |>
    validate_if(is.numeric(year),
                description = "`year` should be numeric") |>
    validate_if(is.numeric(weo_gdp),
                description = "`weo_gdp` should be numeric") |>
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

#' Validate raw weo data
#'
#' @param weo raw weo data, as loaded via `pipfun::load_from_gh`
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#' @keywords internal
#'
#' @export
weo_validate_raw <- function(weo, detail = getOption("pipaux.detail.raw")){

  stopifnot("WEO raw data is not loaded" = !is.null(weo))

  report <- data_validation_report()

  weo <- weo[!is.na(`WEO Subject Code`), ]

  validate(weo, name = "WEO raw data validation") |>
    validate_if(is.character(`WEO Country Code`),
                description = "`WEO Country Code` should be character") |>
    validate_if(is.character(ISO),
                description = "ISO should be character") |>
    validate_if(is.character(`WEO Subject Code`),
                description = "`WEO Subject Code` should be character") |>
    validate_if(is.character(Country),
                description = "`Country` should be character") |>
    validate_if(is.character(`Subject Descriptor`),
                description = "`Subject Descriptor` should be character") |>
    validate_if(is.character(`Subject Notes`),
                description = "`Subject Notes` should be character") |>
    validate_if(is.character(Units),
                description = "`Units` should be character") |>
    validate_if(is.character(Scale),
                description = "`Scale` should be character") |>
    validate_if(is.character(`Country/Series-specific Notes`),
                description = "`Country/Series-specific Notes` should be character") |>
    validate_if(is.numeric(`Estimates Start After`),
                description = "`Estimates Start After` should be numeric") |>
    validate_cols(not_na, ISO, `WEO Subject Code`,
                  description = "no missing values in key variables") |>
    validate_if(is_uniq(ISO, `WEO Subject Code`),
                description = "no duplicate records in key variables") |>
    add_results(report)

  num_var_list <- grep("^[[:digit:]]", colnames(weo))

  for (i in 1:length(num_var_list)) {
    validate(weo, name = "WEO validation") |>
      validate_cols(is.numeric, num_var_list[i],
                    description = "variables (with numeric var name) should be numeric") |>
      add_results(report)
  }

  validation_record <- get_results(report, unnest = FALSE) |>
    setDT()

  if (any(validation_record[["type"]] == "error")){
    get_error_validation(validation_record, detail)
  }

}


