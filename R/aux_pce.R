#' PIP PCE
#'
#' Load or update PCE data.
#'
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @inheritParams aux_gdp
#' @inheritParams aux_pfw
#' @inheritParams pipfun::load_from_gh
#' @export
aux_pce <- function(action  = c("update", "load"),
                    force   = FALSE,
                    owner   = getOption("pipfun.ghowner"),
                    tag     = NULL,
                    detail  = getOption("pipaux.detail.raw")) {

  measure <- "pce"
  action <- match.arg(action)

  wrk_release <- get_from_auxenv(key = "wrk_release")

  release        <- wrk_release$release
  identity       <- wrk_release$identity
  branch         <- paste0(release, "_", identity)

  if (is.null(tag)) {
    tag <- paste0(release, "_", identity)
  }

  if (action == "update") {
    aux_pce_update(force   = force,
                   owner   = owner,
                   branch  = branch,
                   tag     = tag,
                   detail  = detail)

  } else {

    dt <- pipload::load_aux_data(measure = measure)

    return(dt)
  }
}

#' Update PCE
#'
#' Update PCE data using WDI and Special cases.
#'
#' @inheritParams aux_gdp
#' @inheritParams pipfun::load_from_gh
#' @keywords internal
aux_pce_update <- function(force = FALSE,
                           owner   = getOption("pipfun.ghowner"),
                           branch = NULL,
                           tag     = branch,
                           detail  = getOption("pipaux.detail.raw")) {
  measure <- "pce"

  #   ________________________________________________________________
  #   Load data                                             ####
  #

  wpce   <- pipload::load_aux_data(measure = "wdi")

  setnames(wpce, "NE.CON.PRVT.PC.KD", "wdi_pce")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Special national accounts --------
  sna <- pipfun::load_from_gh(
    measure = "sna",
    owner  = owner,
    branch = branch,
    ext    = "csv"
  )

  # validate sna data
  sna_validate_raw(sna = sna, detail = detail)

  sna_fy <- pipfun::load_from_gh(
    measure = "sna",
    owner  = owner,
    branch = branch,
    filename = "sna_metadata",
    ext     = "csv"
  )
  # validate sna_fy data
  sna_fy_validate_raw(sna_fy = sna_fy, detail = detail)
  #   ____________________________________________________________________________
  #   Clean PCE from WDI                                                      ####

  # Keep relevant variables
  wpce <- wpce[, .(country_code, year, wdi_pce)]

  ## ---- Adjust FY to CY ----

  # Merge WDI with special FY cases
  sna_fy <- sna_fy[, c("Code", "Month", "Day")]
  names(sna_fy) <- c("country_code", "fy_month", "fy_day")
  wpce <- merge(wpce, sna_fy, by = "country_code", all.x = TRUE)

  # Calculate alpha
  wpce[, max_days := days_in_month(fy_month, year)]
  wpce[, month_num := get_month_number(fy_month)]
  wpce[, alpha := ((month_num - 1) + fy_day / max_days) / 12]

  # Create lead/lag vars
  wpce[, wdi_pce_lag := shift(wdi_pce), by = country_code]
  wpce[, wdi_pce_lead := shift(wdi_pce, type = "lead"), by = country_code]

  # Calculate adjusted GDP for calendar year
  wpce[,
       wdi_pce_cy := fifelse(!is.na(alpha),
                             fifelse(alpha < 0.5 ,
                                     alpha * wdi_pce_lag + (1 - alpha) * wdi_pce,
                                     alpha * wdi_pce + ( 1 - alpha) *  wdi_pce_lead),
                             NA_real_)
  ]
  wpce[,
       wdi_pce_tmp := fifelse(!is.na(alpha), wdi_pce_cy, wdi_pce)
  ]
  wpce[,
       wdi_pce :=
         fifelse(country_code == "EGY" & year < 1980, # Egypt should only be adjusted after 1980
                 wdi_pce, wdi_pce_tmp)

  ]

  # Keep relevant variables
  pce <- wpce[, .(country_code, year, wdi_pce)]


  #   ____________________________________________________________________________
  #   Special cases                                                           ####

  ## ---- Expand for special cases with U/R levels ----

  # Special cases for IND, IDN, and CHN
  sp <- pce[country_code %in% c("IND", "IDN", "CHN")]

  # Expand two time these cases using cross-join.
  sp <- sp[CJ(
    pce_data_level = c(0, 1),
    country_code = country_code,
    year = year,
    unique = TRUE
  ),
  on = .(country_code, year)
  ]

  # Add data level national to main dataset
  pce[, pce_data_level := 2]

  # Append
  pce <- rbindlist(list(pce, sp))

  # Add domain column
  pce[,
      pce_domain := fifelse(pce_data_level == 2, 1, 2)
  ]

  # Sort
  setorder(pce, country_code, year, pce_data_level)

  ## ---- Recode domain and data level ----

  # Recode domain and data_level variables
  cols <- c("pce_domain", "pce_data_level")
  pce[,
      (cols) := lapply(.SD, as.character),
      .SDcols = cols
  ][
    , # recode domain
    pce_domain := fcase(
      pce_domain == "1", "national",
      pce_domain == "2", "urban/rural",
      pce_domain == "3", "subnational region"
    )
  ][ # Recode data_level only for those that are national or urban/rural
    pce_domain %in% c("national", "urban/rural"),
    pce_data_level := fcase(
      pce_data_level == "0", "rural",
      pce_data_level == "1", "urban",
      pce_data_level == "2", "national"
    )
  ]


  ## ---- Hard-coded custom modifications ----
  # get survey years where only PCE is present
  sna <- sna[!is.na(PCE)
  ][, # lower case coverage
    coverage := tolower(coverage)
  ]

  # If there are special countries
  if (nrow(sna) > 0) {
    # Join with Special National Accounts data.
    setnames(x = sna,
             old = c("countrycode", "coverage"),
             new = c("country_code", "pce_data_level")
    )

    pce[sna,
        on = .(country_code, year, pce_data_level),
        `:=`(
          sna_pce = i.PCE
        )
    ]

    pce[,
        pce := fifelse(is.na(sna_pce),wdi_pce, sna_pce)
    ]
    # remvoe extra variables
    pce[,
        c("sna_pce", "wdi_pce") := NULL]

  } else {
    # If there are no special countries
    setnames(pce, "wdi_pce", "pce")
  }


  #   _______________________________________________________________________
  #   Hard-coded countries                                              ####

  # Remove observations for Venezuela after 2014
  pce[
    ,
    pce := fifelse(country_code == "VEN" & year > 2014, NA_real_, pce)
  ]

  # Remove observations for Belize before 1992
  # See issue PIP-Technical-Team/pipaux#41
  pce[
    ,
    pce := fifelse(country_code == "BLZ" & year < 1992, NA_real_, pce)
  ]

  # Remove all observations for Iraq
  # See issue PIP-Technical-Team/pipaux#43
  pce[
    ,
    pce := fifelse(country_code == "IRQ", NA_real_, pce)
  ]


  #   __________________________________________________________________
  #   Finalize table                                                 ####


  # Remove rows with missing GDP\
  pce <- na.omit(pce, "pce")
  pce <- pce[!is.infinite(pce)]


  # Remove any non-WDI countries
  cl <- pipload::load_aux_data(measure = "country_list")

  pce <- pce[country_code %in% cl$country_code]

  # drop pce_domain
  pce <- pce[, -c("pce_domain")]

  ## ---- Sign and save ----
  pce <- pce |> setnames("pce_data_level", "reporting_level",
                         skip_absent=TRUE)
  
  key_cols = c("country_code", "year", "reporting_level")

  setattr(pce, "aux_name", "pce")
  setattr(pce,
          "aux_key",
          key_cols)

  # validate pce output data
  pce_validate_output(pce = pce, detail = detail)

  if (branch == "main") {
    branch <- ""
  }

  # ----- function raw sha -----------------------------
  raw_sha_fun <- digest::digest(body(
    paste0("aux_", measure))
  )


  setattr(pce,
          "raw_sha_fun",
          raw_sha_fun)


  # Collect gh attribute from sna (main external GH source)
  gh <- attributes(sna)$gh
  saved <-  pip_aux_save(
    x        = pce,
    id       = measure,
    force    = force,
    pk       = key_cols,
    metadata = list(gh = gh),
    code     = aux_pce_update,
    code_label = "aux_pce_update"
  )

  return(invisible(saved))

}

#' Validate output pce data
#'
#' @param pce output pce data
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @import data.validator
#' @importFrom assertr in_set not_na is_uniq
#' @keywords internal
#'
#' @export
pce_validate_output <- function(pce, detail = getOption("pipaux.detail.output")){

  stopifnot("PCE clean data is not loaded" = !is.null(pce))

  report <- data_validation_report()

  validate(pce, name = "PCE output data validation") |>
    validate_if(is.character(country_code),
                description = "`country_code` should be character") |>
    validate_if(is.numeric(year),
                description = "`year` should be numeric") |>
    validate_if(is.numeric(pce),
                description = "`pce` should be numeric") |>
    validate_if(is.character(reporting_level),
                description = "`reporting_level` should be character") |>
    validate_cols(in_set(c("national", "rural", "urban")),
                  reporting_level, description = "`reporting_level` values within range") |>
    # validate_if(is.character(pce_domain),
    #             description = "`pce_domain` should be character") |>
    # validate_cols(in_set(c("national", "urban/rural")),
    #               pce_domain, description = "`pce_domain` values within range") |>
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

