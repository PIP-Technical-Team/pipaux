#' Update PCE
#'
#' Update PCE data using WDI and Special cases.
#'
#' @inheritParams pip_prices
#' @inheritParams pip_wdi_update
#' @keywords internal
pip_pce_update <- function(force = FALSE,
                           maindir = gls$PIP_DATA_DIR,
                           sna_tag = "main",
                           from    = "file") {


#   ____________________________________________________________________________
#   Load data                                                               ####

  if (force) {
    pip_wdi_update(maindir = maindir,
                   from    = from)
  }
  wpce   <- pipload::pip_load_aux("wdi", maindir = maindir)
  setnames(wpce, "NE.CON.PRVT.PC.KD", "wdi_pce")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Special national accounts --------
  usna <- glue("https://github.com/PIP-Technical-Team/pip-sna/raw/{sna_tag}/sna.csv")
  umet <- "https://github.com/PIP-Technical-Team/pip-sna/raw/main/sna_metadata.csv"

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


      if (! (sna_tag  %in% tags)) {
        msg     <- c(
          "{.field sna_tag} specified ({sna_tag}) does not exist in repo
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

  cl     <- pip_country_list("load", maindir = maindir)

  setDT(sna)
  setDT(cl)


#   ____________________________________________________________________________
#   Clean PCE from WDI                                                      ####

  # Keep relevant variables
  wpce <- wpce[, .(country_code, year, wdi_pce)]

  ## ---- Adjust FY to CY ----

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


#   ____________________________________________________________________________
#   Hard-coded countries                                                     ####

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


#   ____________________________________________________________________________
#   Finalize table                                                          ####


  # Remove rows with missing GDP
  pce <- pce[!is.na(pce) & !is.infinite(pce)]


  # Remove any non-WDI countries
  pce <- pce[country_code %in% cl$country_code]

  ## ---- Sign and save ----

  measure <- "pce"
  msrdir <- fs::path(maindir, "_aux/", measure)

  pip_sign_save(
    x = pce,
    measure = measure,
    msrdir = msrdir,
    force = force
  )
}
