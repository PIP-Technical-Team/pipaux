#' Update PCE
#'
#' Update PCE data using WDI and Special cases.
#'
#' @inheritParams pip_prices
#' @export
pip_pce_update <- function(force, maindir = getOption("pipaux.maindir")){

  # ---- Load data ----

  wpce <- wbstats::wb_data(indicator = "NE.CON.PRVT.PC.KD", lang = "en")
  sna <- readxl::read_xlsx(sprintf('%s_aux/sna/NAS special_2021-01-14.xlsx', maindir))
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
  pce <- wpce[,
              .(country_code, year, wdi_pce)
  ]

  # ---- Hard-coded custom modifications ----

  # Join with Special National Accounts data.
  sna <- sna[countrycode == 'IND']
  setnames(sna, "countrycode", "country_code")
  pce[sna,
      on = .(country_code, year),
      `:=`(
        sna_pce = i.PCE
      )
  ]

  # India should be replaced with country specific-sources from 2011
  pce[,
      pce := fifelse(country_code == "IND" & year > 2010,
                     sna_pce,
                     wdi_pce)
  ]
  pce$sna_pce <- NULL
  pce$wdi_pce <- NULL

  # Remove observations for Venezuela after 2014
  pce[,
      pce := fifelse(country_code == "VEN" & year > 2014, NA_real_, pce)
  ]

  # ---- Expand for special cases with U/R levels ----

  # Special cases for IND, IDN, and CHN
  sp <- pce[country_code %chin% c("IND", "IDN", "CHN")]

  # Expand two time these cases using cross-join.
  sp <- sp[CJ(pce_data_level   = c(0, 1),
              country_code = country_code,
              year         = year,
              unique       = TRUE),
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

  # ---- Finalize table ----

  # Remove rows with missing GDP
  pce <- pce[!is.na(pce) & !is.infinite(pce)]

  # Recode domain and data_level variables
  cols <- c("pce_domain", "pce_data_level")
  pce[,
      (cols) := lapply(.SD, as.character),
      .SDcols = cols
  ][,# recode domain
    pce_domain := fcase(
      pce_domain == "1", "national",
      pce_domain == "2", "urban/rural",
      pce_domain == "3", "subnational region"
    )
  ][   # Recode data_level only for those that are national or urban/rural
    pce_domain %in% c("national", "urban/rural"),
    pce_data_level := fcase(
      pce_data_level == "0", "rural",
      pce_data_level == "1", "urban",
      pce_data_level == "2", "national"
    )
  ]

  # Remove any non-WDI countries
  pce <- pce[country_code %in% cl$country_code]

  # ---- Sign and save ----

  measure <- "pce"
  msrdir  <- paste0(maindir, "_aux/", measure, "/")

  pip_sign_save(x       = pce,
                measure = measure,
                msrdir  = msrdir,
                force   = force)

}
