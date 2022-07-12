#' Update GDP
#'
#' Update GDP data using WDI, Maddison and Special cases.
#'
#' @inheritParams pip_gdp
#' @inheritParams pip_wdi_update
#' @inheritParams load_raw_aux
#' @keywords internal
pip_gdp_update <- function(maindir = gls$PIP_DATA_DIR,
                           force   = FALSE,
                           owner   = getOption("pipaux.ghowner"),
                           branch  = c("DEV", "PROD", "main"),
                           tag     = match.arg(branch),
                           from    = "file") {

  branch <- match.arg(branch)

  # Update Maddison Project Data
  pip_maddison(force = force,
               maindir = maindir,
               branch = branch)

  # Update WEO data

  pip_weo(force = force,
          maindir = maindir,
          branch = branch)


  #----------------------------------------------------------
  #   Load data
  #----------------------------------------------------------

  madd   <- load_aux(measure = "maddison",
                     maindir = maindir,
                     branch = branch)


  weo    <-  load_aux(measure = "weo",
                      maindir = maindir,
                      branch = branch)


  if (force) {
    pip_wdi_update(maindir = maindir,
                   from    = from)
  }
  wgdp   <- pipload::pip_load_aux("wdi", maindir = maindir)
  setnames(wgdp, "NY.GDP.PCAP.KD", "wdi_gdp")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Special national accounts --------
  usna <- glue("https://github.com/PIP-Technical-Team/pip-sna/raw/{sna_branch}/sna.csv")
  umet <- glue("https://github.com/PIP-Technical-Team/pip-sna/raw/{sna_branch}/sna_metadata.csv")

  tryCatch(
    expr = {
      # Your code...
      sna <- suppressMessages(
        readr::read_csv(usna)
      )
    }, # end of expr section

    error = function(e) {
      owner <-  "pip-technical-team"
      repo  <-  "pip-sna"
      tags  <- c("main", get_gh_tags(owner, repo))


      if (! (sna_branch  %in% tags)) {
        msg     <- c(
          "{.field sna_branch} specified ({sna_branch}) does not exist in repo
          {.file {owner}/{repo}}",
          "i" = "Select one among {.field {tags}}"
          )
        cli::cli_abort(msg, class = "pipaux_error")

      } else {
        msg     <- c("Could not load sna from Github repo:
                     {e$message}")
        cli::cli_abort(msg,class = "pipaux_error")

      }
    } # end of finally section

  ) # End of trycatch

  sna_fy <- suppressMessages(
    readr::read_csv(umet)
  )

  cl <- pip_country_list("load", maindir = maindir)

  setDT(madd)
  setDT(weo)
  setDT(sna)
  setDT(cl)

  #--------- Clean GDP from WDI ---------
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
  sna <- sna[!is.na(GDP)]

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

  measure <- "gdp"
  msrdir <- fs::path(maindir, "_aux/", measure)

  pip_sign_save(
    x = gdp,
    measure = measure,
    msrdir = msrdir,
    force = force
  )
}
