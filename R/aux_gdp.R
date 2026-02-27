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
    aux_gdp_update(owner   = owner,
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

    # Collect gh attribute from loaded gdp
    gh <- attributes(gdp)$gh

    if (branch == "main") {
      branch <- ""
    }

    key_cols <- c("country_code", "reporting_level", "year")

    # setattr(gdp, "aux_key", key_cols)
    saved <- pip_aux_save(
      x        = gdp,
      id       = measure,
      pk       = key_cols,
      metadata = list(gh = gh),
      code     = aux_gdp_update,
      code_label = "aux_gdp_update"
    )

  } else {

    dt <- pipload::load_aux_data(measure = measure)

    return(dt)
  }
} # end of aux_gdp

#' Update GDP
#'
#' Update GDP data using WDI, Maddison and Special cases.
#'
#' @inheritParams aux_gdp
#' @inheritParams pipfun::load_from_gh
#' @keywords internal
aux_gdp_update <- function(owner   = getOption("pipfun.ghowner"),
                           branch  = NULL,
                           tag     = branch,
                           detail  = getOption("pipaux.detail.raw")) {

  #branch <- match.arg(branch)
  measure <- "gdp"

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

