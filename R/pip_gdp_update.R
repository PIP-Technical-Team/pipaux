#' Update GDP
#'
#' Update GDP data using WDI, Maddison and Special cases.
#'
#' @inheritParams pip_gdp
#' @keywords intenal
pip_gdp_update <- function(force = FALSE, maindir = gls$PIP_DATA_DIR) {

  #----------------------------------------------------------
  #   Load data
  #----------------------------------------------------------

  madd   <- pip_maddison("load", maindir = maindir)
  weo    <- pip_gdp_weo("load", maindir = maindir)
  wgdp   <- wbstats::wb_data(indicator = "NY.GDP.PCAP.KD", lang = "en")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Special national accounts --------

  sna_files <- fs::dir_ls(fs::path(maindir, "_aux/sna"),
                          type = "file",
                          regexp = "^NAS special_.*xlsx")

  sna_files <- fs::dir_ls(fs::path(maindir, "_aux/sna"),
                          regexp = "NAS special_.*")

  sna_vintages <- gsub("(.*NAS special_)(.*)(\\.xlsx)", "\\2", sna_files)
  sna_vintages <- as.Date(sna_vintages, "%Y-%m-%d")
  sna_last     <- which(sna_vintages == max(sna_vintages))

  sna_touse    <- sna_files[sna_last]

  sna    <- readxl::read_xlsx(sna_touse)
  sna_fy <- readxl::read_xlsx(fs::path(maindir, "_aux/sna/National_Accounts_Fiscal_Years_Metadata.xlsx"),
                              sheet = "WDI Jan2022")
  cl <- pip_country_list("load", maindir = maindir)

  setDT(madd)
  setDT(wgdp)
  setDT(weo)
  setDT(sna)
  setDT(cl)

  #--------- Clean GDP from WDI ---------

  # Rename columns
  setnames(wgdp,
    old = c("iso3c", "date", "NY.GDP.PCAP.KD"),
    new = c("country_code", "year", "wdi_gdp")
  )

  # Keep relevant variables
  wgdp <- wgdp[, .(country_code, year, wdi_gdp)]

  # ---- Adjust FY to CY ----

  # Merge WDI with special FY cases
  sna_fy <- sna_fy[c("Code", "Month", "Day")]
  names(sna_fy) <- c("country_code", "fy_month", "fy_day")
  wgdp <- merge(wgdp, sna_fy, by = "country_code", all.x = TRUE)

  # Calculate alpha
  wgdp[, max_days := days_in_month(fy_month, year)]
  wgdp[, month_num := get_month_number(fy_month)]
  wgdp[, alpha := ((month_num - 1) + fy_day / max_days) / 12]

  # Create lead/lag vars
  wgdp[, wdi_gdp_lag := dplyr::lag(wdi_gdp), by = country_code]
  wgdp[, wdi_gdp_lead := dplyr::lead(wdi_gdp), by = country_code]

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
         fifelse(country_code == "EGY" & year < 1980, # Egypt should only be adjusted after 1980
                 wdi_gdp, wdi_gdp_tmp)

  ]

  # Keep relevant variables
  wgdp <- wgdp[, .(country_code, year, wdi_gdp)]


  #----------------------------------------------------------
  #   Merge WDI GDP data with other sources
  #----------------------------------------------------------


  # Merge Maddison and WDI (full join)
  gdp <- data.table::merge.data.table(
    wgdp, madd,
    by = c("country_code", "year"),
    all = TRUE
  )

  # Merge WEO and WDI (full join)
  gdp <- data.table::merge.data.table(
    gdp, weo,
    by = c("country_code", "year"),
    all = TRUE
  )

  # Chain in following order 1) WDI, 2) WEO, 3) Maddison

  # Chain WEO on WDI
  gdp <- chain_values(
    gdp,
    base_var = "wdi_gdp",
    replacement_var = "weo_gdp",
    new_name = "new_gdp",
    by = "country_code"
  )

  # Chain Maddison on new GDP column
  gdp <- chain_values(
    gdp,
    base_var = "new_gdp",
    replacement_var = "mpd_gdp",
    new_name = "gdp",
    by = "country_code"
  )

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
  sna <- sna[countrycode == "SYR"]
  setnames(sna, "countrycode", "country_code")
  gdp[sna,
    on = .(country_code, year),
    `:=`(
      chain_factor = i.GDP
    )
  ]

  # Modify observations for Syria after 2010
  syr_2010 <- gdp[country_code == "SYR" & year == 2010]$gdp
  gdp[
    ,
    gdp := fifelse(
      country_code == "SYR" & year > 2010,
      syr_2010 * chain_factor,
      gdp
    )
  ]
  gdp$chain_factor <- NULL


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

  # Remove any non-WDI countries
  gdp <- gdp[country_code %in% cl$country_code]

  # ---- Save and sign ----

  measure <- "gdp"
  msrdir <- fs::path(maindir, "_aux/", measure)

  pip_sign_save(
    x = gdp,
    measure = measure,
    msrdir = msrdir,
    force = force
  )
}
