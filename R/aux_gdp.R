#' PIP GDP
#'
#' Update or load GDP data.
#'
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @inheritParams aux_pfw
#' @inheritParams pipfun::load_from_gh
#' @param from character: Either "gh", "file" or "api". Default is "gh". "file"
#'   and "gh" are synonymous
#' @export
aux_gdp <- function(action          = c("update", "load"),
                    force           = FALSE,
                    owner           = getOption("pipfun.ghowner"),
                    tag             = NULL,
                    detail          = getOption("pipaux.detail.raw")) {

  measure    <- "gdp"

  wrk_release <- get_from_auxenv(key = "wrk_release")

  release        <- wrk_release$release
  identity       <- wrk_release$identity
  branch         <- paste0(release, "_", identity)

  if (is.null(tag)) {
    tag <- paste0(release, "_", identity)
  }

  action     <- match.arg(action)


  if (action == "update") {

    # Get raw data from various sources, format it, and push it to github
    aux_gdp_update(force   = force,
                   owner   = owner,
                   branch  = branch,
                   tag     = tag,
                   detail  = detail)

    # load raw data from gh together with its metadata

    gdp <- pipfun::load_from_gh(
      measure = "gdp",
      owner  = owner,
      branch = branch,
      ext = "csv"
    )


    if (branch == "main") {
      branch <- ""
    }

    # ----- function raw sha ------
    raw_sha_fun <- digest::digest(body(
      paste0("aux_", measure))
    )

    setattr(gdp,
            "raw_sha_fun",
            raw_sha_fun)


    key_cols <- c("country_code", "reporting_level", "year")
    setattr(gdp, "aux_key", key_cols)
    saved <- pip_aux_save(
      x        = gdp,
      id       = measure,
      force    = force,
      pk       = key_cols
    )

  } else {

    dt <- pipload::load_aux_data(measure = measure)

    return(dt)
  }
} # end of aux_gdp

#' Fetch GDP data from WEO
#'
#' Create a dataset with GDP data from World Economic Outlook.
#'
#' Note that the most recent version most be downloaded from imf.org and saved
#' as an .xls file in `<maindir>/_aux/weo/`. The filename should be in the
#' following structure `WEO_<YYYY-DD-MM>.xls`. Due to potential file corruption
#' the file must be opened and re-saved before it can be updated with
#' `aux_gdp_weo()`. Hopefully in the future IMF will stop using an `.xls` file
#' that's not really xls.
#'
#' @export
aux_gdp_weo <- function(action = "update",
                        force = FALSE,
                        maindir = getOption("pipaux.working_dir")) {
  measure <- "weo"
  msrdir <- fs::path(maindir, "aux_data/", measure) # measure dir

  if (action == "update") {

    # ---- Load data from disk ----

    # Get latest version of file (in case there are more)
    dir <- sprintf("%s_aux/weo/", maindir)
    weo_files <- list.files(dir, pattern = "WEO_.*[.]xls")
    weo_latest <- weo_files %>%
      gsub("WEO_|.xls", "", .) %>%
      as.POSIXlt() %>%
      max() %>%
      as.character() %>%
      sprintf("%s_aux/weo/WEO_%s.xls", maindir, .)

    # Read data
    dt <- readxl::read_xls(
      weo_latest,
      sheet = 1, na = "n/a",
      col_types = "text"
    )
    dt <- setDT(dt)

    # Clean column names
    dt <- janitor::clean_names(dt)

    # ---- Data transformations ----

    # Select rows w/ data on real gdp per capita
    dt <- dt[weo_subject_code %in%
               c("NGDPRPC", "NGDPRPPPPC", "NGDP_R")]

    # Fix country codes
    dt[
      ,
      iso := fifelse(
        iso == "WBG", "PSE", iso # West Bank & Gaza
      )
    ]
    dt[
      ,
      iso := fifelse(
        iso == "UVK", "XKX", iso # Kosovo
      )
    ]

    # Replace subject codes
    dt[
      ,
      subject_code := fcase(
        weo_subject_code == "NGDPRPC", "weo_gdp_lcu",
        weo_subject_code == "NGDPRPPPPC", "weo_gdp_ppp2017",
        weo_subject_code == "NGDP_R", "weo_gdp_lcu_notpc"
      )
    ]

    # Reshape to long format
    dt <- dt %>%
      melt(
        id.vars = c("iso", "subject_code"),
        measure.vars = names(dt)[grepl("\\d{4}", names(dt))],
        value.name = "weo_gdp", variable.name = "year"
      )
    setnames(dt, "iso", "country_code")

    # Convert year and GDP to numeric
    dt$year <- sub("x", "", dt$year) %>% as.numeric()
    dt$weo_gdp <- suppressWarnings(as.numeric(dt$weo_gdp))

    # Remove rows w/ missing GDP
    dt <- dt[!is.na(dt$weo_gdp)]

    # Remove current year and future years
    current_year <- format(Sys.Date(), "%Y")
    dt <- dt[dt$year < current_year]

    # Reshape to wide for GDP columns
    dt <- dt %>%
      dcast(
        formula = country_code + year ~ subject_code,
        value.var = "weo_gdp"
      )

    # ---- Merge with population ----

    pop <- aux_pop("load", maindir = maindir)
    setDT(pop)
    pop <- pop[pop_data_level == "national", ]
    dt[pop,
       on = .(country_code, year),
       `:=`(
         pop = i.pop
       )
    ]

    # Calculate per capita value for NGDP_R
    dt[
      ,
      weo_gdp_lcu := fifelse(
        is.na(weo_gdp_lcu), weo_gdp_lcu_notpc / pop, weo_gdp_lcu
      )
    ]


    # ---- Chain PPP and LCU GDP columns ----

    # Chain LCU on PPP column
    dt <- chain_values(
      dt,
      base_var = "weo_gdp_ppp2017",
      replacement_var = "weo_gdp_lcu",
      new_name = "weo_gdp",
      by = "country_code"
    )


    # --- Sign and save ----

    # Select final columns
    dt <- dt[, c("country_code", "year", "weo_gdp")]

    # Save dataset
    # ----- function raw sha ------
    raw_sha_fun <- digest::digest(body(
      paste0("aux_", measure))
    )

    setattr(dt, "aux_name", "pfw")

    setattr(dt,
            "raw_sha_fun",
            raw_sha_fun)

    # aux_sign_save(
    #   x = dt,
    #   measure = measure,
    #   msrdir = msrdir,
    #   force = force
    # )

    saved <- pipfun::pip_sign_save(
      x       = dt,
      measure = measure,
      msrdir  = msrdir,
      force   = force
    )

    return(
      invisible(saved)
    )


  } else if (action == "load") {
    dt <- load_aux(
      maindir = maindir,
      measure = measure
    )
    return(dt)
  } else {
    rlang::abort(c("`action` must be `update` or `load`",
                   x = paste0("you provided `", action, "`")
    ))
  }
}






#' Update GDP
#'
#' Update GDP data using WDI, Maddison and Special cases.
#'
#' @inheritParams aux_gdp
#' @inheritParams pipfun::load_from_gh
#' @keywords internal
aux_gdp_update <- function(force   = FALSE,
                           owner   = getOption("pipfun.ghowner"),
                           branch  = NULL,
                           tag     = branch,
                           detail  = getOption("pipaux.detail.raw")) {

  #branch <- match.arg(branch)
  measure <- "gdp"

  #   _________________________________________
  #   Update data                                 ####

  # # Update Maddison Project Data
  # pip_maddison(force   = force,
  #              maindir = maindir,
  #              branch  = branch)
  #
  # # Update WEO data
  #
  # pip_weo(force   = force,
  #         maindir = maindir,
  #         branch  = branch)
  #
  # # Update WDI
  # pip_wdi_update(maindir = maindir,
  #                from    = from,
  #                force   = force,
  #                branch  = branch)
  #

  #   ____________________________________________________________________________
  #   Load Data                                                               ####

  madd   <- pipload::load_aux_data(measure = "maddison")

  weo    <-  pipload::load_aux_data(measure = "weo")


  wgdp   <- pipload::load_aux_data(measure = "wdi")

  setnames(wgdp, "NY.GDP.PCAP.KD", "wdi_gdp")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Special national accounts --------
  sna <- pipfun::load_from_gh(
    measure = "sna",
    owner  = owner,
    branch = branch,
    ext    = "csv"
  )
  # validate sna data
  sna_validate_raw(sna, detail = detail)

  sna_fy <- pipfun::load_from_gh(
    measure = "sna",
    owner  = owner,
    branch = branch,
    filename = "sna_metadata",
    ext     = "csv"
  )

  # load nowcast growth rates
  nan <- pipfun::load_from_gh(
    measure = "nan",
    owner  = "PIP-Technical-Team",
    branch = branch,
    ext    = "csv"
  )


  cl <- pipload::load_aux_data(measure = "country_list")


  #   ____________________________________________________________________________
  #   Clean data                                                              ####

  ##--------- Clean GDP from WDI ---------
  # Keep relevant variables
  wgdp <- wgdp[, .(country_code, year, wdi_gdp)]

  # ---- Adjust FY to CY ----

  # Merge WDI with special FY cases
  sna_fy <- sna_fy[, c("Code", "Month", "Day")]
  names(sna_fy) <- c("country_code", "fy_month", "fy_day")
  wgdp <- merge(wgdp, sna_fy, by = "country_code", all.x = TRUE)

  # Calculate alpha
  wgdp[, max_days := days_in_month(fy_month, year)]
  wgdp[, month_num := get_month_number(fy_month)]
  wgdp[, alpha := ((month_num - 1) + fy_day / max_days) / 12]

  # Create lead/lag vars
  wgdp[,
       wdi_gdp_lag := shift(wdi_gdp),
       by = country_code]
  wgdp[,
       wdi_gdp_lead := shift(wdi_gdp, type = "lead"),
       by = country_code]

  # Calculate adjusted GDP for calendar year
  wgdp[,
       wdi_gdp_cy := fifelse(!is.na(alpha),
                             fifelse(alpha < 0.5 ,
                                     alpha * wdi_gdp_lag + (1 - alpha) * wdi_gdp,
                                     alpha * wdi_gdp + ( 1 - alpha) *  wdi_gdp_lead),
                             NA_real_)
  ]
  wgdp[,
       wdi_gdp_tmp := fifelse(!is.na(alpha), wdi_gdp_cy, wdi_gdp)
  ]
  wgdp[,
       wdi_gdp :=
         # Egypt should only be adjusted after 1980
         fifelse(country_code == "EGY" & year < 1980,
                 wdi_gdp, wdi_gdp_tmp)

  ]

  # Keep relevant variables
  wgdp <- wgdp[, .(country_code, year, wdi_gdp)]


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #   Merge WDI GDP data with other sources -------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  # Merge Maddison and WDI (full join)
  gdp <- merge.data.table(
    wgdp, madd,
    by = c("country_code", "year"),
    all = TRUE
  )

  # Merge WEO and WDI (full join)
  gdp <- merge.data.table(
    gdp, weo,
    by = c("country_code", "year"),
    all = TRUE
  )

  # Chain in following order 1) WDI, 2) WEO, 3) Maddison

  # Chain WEO on WDI
  gdp[, new_gdp := chain_val(ori_var = wdi_gdp,
                             rep_var = weo_gdp),
      by = country_code]

  # Chain Maddison on new GDP column
  gdp[, gdp := chain_val(ori_var = new_gdp,
                         rep_var = mpd_gdp),
      by = country_code]


  # Select columns
  gdp <- gdp[, c("country_code", "year", "gdp")]

  # ---- Hard-coded custom modifications ----

  # Remove observations for Venezuela after 2014
  gdp[
    ,
    gdp := fifelse(country_code == "VEN" & year > 2014, NA_real_, gdp)
  ]

  # Syria should be replaced with country specific-sources from 2010

  # Merge with sna
  sna <- na.omit(sna, "GDP")


  # If there are special countries
  if (nrow(sna) > 0) {
    # Join with Special National Accounts data.
    setnames(sna, "countrycode", "country_code")

    gdp[sna,
        on = .(country_code, year),
        `:=`(
          sna_gdp = i.GDP
        )
    ]

    gdp[,
        gdp := fifelse(is.na(sna_gdp),gdp, sna_gdp)
    ]
    # remove extra variables
    gdp[,
        sna_gdp := NULL]
  }

  # ---- Expand for special cases with U/R levels ----

  # Special cases for IND, IDN, and CHN
  sp <- gdp[country_code %chin% c("IND", "IDN", "CHN")]

  # Expand two time these cases using cross-join.
  sp <- sp[CJ(
    gdp_data_level = c(0, 1),
    country_code = country_code,
    year = year,
    unique = TRUE
  ),
  on = .(country_code, year)
  ]

  # Add data level national to main dataset
  gdp[, gdp_data_level := 2]

  # Append
  gdp <- rbindlist(list(gdp, sp))

  # Add domain column
  gdp[
    ,
    gdp_domain := fifelse(gdp_data_level == 2, 1, 2)
  ]

  # Sort
  setorder(gdp, country_code, year, gdp_data_level)

  # ---- Finalize table ----

  # Remove rows with missing GDP
  gdp <- gdp[!is.na(gdp) & !is.infinite(gdp)]

  # Recode domain and data_level variables
  cols <- c("gdp_domain", "gdp_data_level")
  gdp[,
      (cols) := lapply(.SD, as.character),
      .SDcols = cols
  ][
    , # recode domain
    gdp_domain := fcase(
      gdp_domain == "1", "national",
      gdp_domain == "2", "urban/rural",
      gdp_domain == "3", "subnational region"
    )
  ][ # Recode data_level only for those that are national or urban/rural
    gdp_domain %in% c("national", "urban/rural"),
    gdp_data_level := fcase(
      gdp_data_level == "0", "rural",
      gdp_data_level == "1", "urban",
      gdp_data_level == "2", "national"
    )
  ]


  # add nowcast growth rates ----------
  byvars <- c("country_code", "gdp_data_level")

  # Find the latest GDP data year for each country
  latest_gdp <- gdp[, .(last_year = max(year),
                        last_gdp = gdp[which.max(year)]),
                    by =  c(byvars, "gdp_domain")]

  # Join this with growth rates or years after the last available GDP year
  dt_growth <- joyn::joyn(nan, latest_gdp,
                          by =  byvars,
                          match_type = "m:1",
                          keep = "left",
                          reportvar = FALSE,
                          verbose = FALSE) |>
    fsubset(year > last_year)

  # Prepare for cumulative growth calculation
  dt_growth[, c("initial_year", "initial_gdp") := .(last_year[1], last_gdp[1]),
            by = byvars]

  # Calculate projected GDP
  # Calculate cumulative GDP projections
  dt_growth[, cum_growth := cumprod(1 + gdppc_growth),
            by = byvars
  ][, projected_GDP := last_gdp * cum_growth
  ]

  # Select the relevant columns for the result
  gdp <- dt_growth |>
    fselect(country_code, gdp_data_level, gdp_domain, year, gdp = projected_GDP) |>
    # append to actual GDP
    rowbind(gdp, fill = TRUE) |>
    setorder(country_code, gdp_data_level, year)

  # Remove any non-WDI countries
  gdp <- gdp[country_code %in% cl$country_code]

  # drop gdp_domain
  gdp <- gdp[, -c("gdp_domain")]

  # ---- Save and sign ----
  gdp <- gdp |> setnames("gdp_data_level", "reporting_level",
                         skip_absent=TRUE)

  setattr(gdp, "aux_name", "gdp")
  setattr(gdp,
          "aux_key",
          c("country_code", "year", "reporting_level"))

  # validate gdp output data
  gdp_validate_output(gdp = gdp, detail = detail)

  if (branch == "main") {
    branch <- ""
  }

  save_aux_to_gh(df        = gdp,
                 owner     = owner,
                 measure   = measure,
                 repo      = paste0("aux_", measure),
                 tag       = tag,
                 branch    = branch,
                 filename  = measure)
  # All aux files that depend on gdp will be loaded from Github
  return(invisible(TRUE))

}

#' Validate output gdp data
#'
#' @param gdp output gdp data
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#' @keywords internal
#'
#' @export
gdp_validate_output <- function(gdp, detail = getOption("pipaux.detail.output")){

  stopifnot("GDP output data is not loaded" = !is.null(gdp))

  report <- data_validation_report()

  validate(gdp, name = "GDP output data validation") |>
    validate_if(is.character(country_code),
                description = "`country_code` should be character") |>
    validate_if(is.numeric(year),
                description = "`year` should be numeric") |>
    validate_if(is.character(reporting_level),
                description = "`reporting_level` should be character") |>
    validate_cols(in_set(c("national", "rural", "urban")),
                  reporting_level, description = "`reporting_level` values within range") |>
    validate_if(is.numeric(gdp),
                description = "`gdp` should be numeric") |>
    # validate_if(is.character(gdp_domain),
    #             description = "`gdp_domain` should be character") |>
    # validate_cols(in_set(c("national", "urban/rural")),
    #               gdp_domain, description = "`gdp_domain` values within range") |>
    validate_cols(not_na, country_code, year, reporting_level,
                  description = "no missing values in key variables") |>
    validate_if(is_uniq(country_code, year, reporting_level),
                description = "no duplicate records in key variables") |>
    add_results(report)

  validation_record <- get_results(report, unnest = FALSE) |>
    setDT()

  if (any(validation_record[["type"]] == "error")){
    get_error_validation(validation_record, detail)
  }}

