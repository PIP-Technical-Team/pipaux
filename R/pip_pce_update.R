#' Update PCE
#'
#' Update PCE data using WDI and Special cases.
#'
#' @inheritParams pip_prices
#' @keywords internal
pip_pce_update <- function(force = FALSE, maindir = gls$PIP_DATA_DIR) {

  # ---- Load data ----

  wpce <- wbstats::wb_data(indicator = "NE.CON.PRVT.PC.KD", lang = "en")
  sna <- readxl::read_xlsx(sprintf("%s_aux/sna/NAS special_2021-01-14.xlsx", maindir))
  sna_fy <- readxl::read_xlsx(sprintf("%s_aux/sna/National_Accounts_Fiscal_Years_Metadata.xlsx", maindir),
                              sheet = "WDI Jan2022")
  cl <- pip_country_list("load", maindir = maindir)

  setDT(wpce)
  setDT(sna)
  setDT(cl)

  # ---- Clean PCE from WDI ----

  # Rename vars
  setnames(wpce,
    old = c("iso3c", "date", "NE.CON.PRVT.PC.KD"),
    new = c("country_code", "year", "wdi_pce")
  )

  # Keep relevant variables
  wpce <- wpce[, .(country_code, year, wdi_pce)]

  # ---- Adjust FY to CY ----

  # Merge WDI with special FY cases
  sna_fy <- sna_fy[c("Code", "Month", "Day")]
  names(sna_fy) <- c("country_code", "fy_month", "fy_day")
  wpce <- merge(wpce, sna_fy, by = "country_code", all.x = TRUE)

  # Calculate alpha
  wpce[, max_days := days_in_month(fy_month, year)]
  wpce[, month_num := get_month_number(fy_month)]
  wpce[, alpha := ((month_num - 1) + fy_day / max_days) / 12]

  # Create lead/lag vars
  wpce[, wdi_pce_lag := dplyr::lag(wdi_pce), by = country_code]
  wpce[, wdi_pce_lead := dplyr::lead(wdi_pce), by = country_code]

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


  # ---- Expand for special cases with U/R levels ----

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
  pce[
    ,
    pce_domain := fifelse(pce_data_level == 2, 1, 2)
  ]

  # Sort
  setorder(pce, country_code, year, pce_data_level)

  # ---- Recode domain and data level ----

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


  # ---- Hard-coded custom modifications ----

  # Join with Special National Accounts data.
  sna <- sna[countrycode == "IND"]
  sna$coverage <- tolower(sna$coverage)
  setnames(
    sna, c("countrycode", "coverage"),
    c("country_code", "pce_data_level")
  )
  pce[sna,
    on = .(country_code, year, pce_data_level),
    `:=`(
      sna_pce = i.PCE
    )
  ]

  # India should be replaced with country specific-sources from 2011
  pce[
    ,
    pce := fifelse(
      country_code == "IND" & year > 2010 &
        pce_data_level != "national",
      sna_pce,
      wdi_pce
    )
  ]
  pce$sna_pce <- NULL
  pce$wdi_pce <- NULL

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

  # ---- Finalize table ----

  # Remove rows with missing GDP
  pce <- pce[!is.na(pce) & !is.infinite(pce)]


  # Remove any non-WDI countries
  pce <- pce[country_code %in% cl$country_code]

  # ---- Sign and save ----

  measure <- "pce"
  msrdir <- paste0(maindir, "_aux/", measure, "/")

  pip_sign_save(
    x = pce,
    measure = measure,
    msrdir = msrdir,
    force = force
  )
}
