#' Update GDP
#'
#' Update GDP data using WDI, Maddison and Special cases.
#'
#' @inheritParams pip_gdp
#' @inheritParams pipfun::load_from_gh
#' @keywords internal
pip_gdp_update <- function(maindir = gls$PIP_DATA_DIR,
                           force   = FALSE,
                           owner   = getOption("pipfun.ghowner"),
                           branch  = c("DEV", "PROD", "main"),
                           tag     = match.arg(branch),
                           from    = c("gh", "file", "api"),
                           detail  = getOption("pipaux.detail.raw")) {

  branch <- match.arg(branch)
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

  madd   <- load_aux(measure = "maddison",
                     maindir = maindir,
                     branch = branch)


  weo    <-  load_aux(measure = "weo",
                      maindir = maindir,
                      branch = branch)


  wgdp   <- load_aux(measure = "wdi",
                     maindir = maindir,
                     branch = branch)

  setnames(wgdp, "NY.GDP.PCAP.KD", "wdi_gdp")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Special national accounts --------
  sna <- pipfun::load_from_gh(
    measure = "sna",
    owner  = owner,
    branch = branch
  )
  # validate sna data
  sna_validate_raw(sna, detail = detail)

  sna_fy <- pipfun::load_from_gh(
    measure = "sna",
    owner  = owner,
    branch = branch,
    filename = "sna_metadata"
  )
  # validate sna_fy data
  sna_fy_validate_raw(sna_fy, detail = detail)

  cl <- load_aux(maindir = maindir,
                 measure = "country_list",
                 branch = branch)


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


  # gdp <- chain_values(
  #   gdp,
  #   base_var = "wdi_gdp",
  #   replacement_var = "weo_gdp",
  #   new_name = "new_gdp",
  #   by = "country_code"
  # )

  # Chain Maddison on new GDP column
  gdp[, gdp := chain_val(ori_var = new_gdp,
                     rep_var = mpd_gdp),
      by = country_code]

  # gdp <- chain_values(
  #   gdp,
  #   base_var = "new_gdp",
  #   replacement_var = "mpd_gdp",
  #   new_name = "gdp",
  #   by = "country_code"
  # )

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
          chain_factor = i.GDP
        )
    ]

    syr_2010 <- gdp[country_code == "SYR" & year == 2010,
                    gdp]
    gdp[,
        gdp := fifelse(is.na(chain_factor),gdp, syr_2010 * chain_factor)
    ]
    # remove extra variables
    gdp[,
        chain_factor := NULL]
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

  # Remove any non-WDI countries
  gdp <- gdp[country_code %in% cl$country_code]

  # ---- Save and sign ----
  # validate gdp output data
  gdp_validate_output(gdp = gdp, detail = detail)

  if (branch == "main") {
    branch <- ""
  }
  msrdir <- fs::path(maindir, "_aux", branch, measure) # measure dir

  saved <- pipfun::pip_sign_save(
    x       = gdp,
    measure = measure,
    msrdir  = msrdir,
    force   = force
  )

  return(invisible(saved))

}
