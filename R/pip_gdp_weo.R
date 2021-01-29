#' Fetch GDP data from WEO
#'
#' Create a dataset with GDP data from World Economic Outlook.
#'
#' Note that the most recent version most be downloaded from imf.org and saved
#' as an .xls file in `<maindir>/_aux/weo/`. The filename should be in the
#' following structure `WEO_<YYYY-DD-MM>.xls`. Due to potential file corruption
#' the file must be opened and re-saved before it can be updated with
#' `pip_gdp_weo()`. Hopefully in the future IMF will stop using an `.xls` file
#' that's not really xls.
#'
#' @inheritParams pip_prices
#' @export
pip_gdp_weo <- function(action = "update",
                        force  = FALSE,
                        maindir = getOption("pipaux.maindir")) {

  measure <- "weo"
  msrdir  <- paste0(maindir, "_aux/", measure, "/") # measure dir

  if (action == "update") {

    # ---- Load data from disk ----

    # Get latest version of file (in case there are more)
    dir <- sprintf('%s_aux/weo/', maindir)
    weo_files <- list.files(dir, pattern = 'WEO_.*[.]xls')
    weo_latest <- weo_files %>%
      gsub("WEO_|.xls", "", .) %>%
      as.POSIXlt() %>%
      max() %>%
      as.character() %>%
      sprintf("%s_aux/weo/WEO_%s.xls", maindir, . )

    # Read data
    dt <- readxl::read_xls(
      weo_latest, sheet = 1, na = 'n/a',
      col_types = 'text')
    dt <- setDT(dt)

    # Clean column names
    dt <- janitor::clean_names(dt)

    # ---- Data transformations ----

    # Select rows w/ data on real gdp per capita
    dt <- dt[weo_subject_code %in%
               c("NGDPRPC", "NGDPRPPPPC", "NGDP_R")]

    # Fix country codes
    dt[,
       iso := fifelse(
         iso == 'WBG', 'PSE', iso # West Bank & Gaza
       )
    ]
    dt[,
       iso := fifelse(
         iso == 'UVK', 'XKX', iso # Kosovo
       )
    ]

    # Replace subject codes
    dt[,
       subject_code := fcase(
         weo_subject_code == "NGDPRPC", 'weo_gdp_lcu',
         weo_subject_code == "NGDPRPPPPC", 'weo_gdp_ppp2017',
         weo_subject_code == "NGDP_R", 'weo_gdp_lcu_notpc'
       )
    ]

    # Reshape to long format
    dt <- dt %>%
      melt(id.vars = c('iso', 'subject_code'),
           measure.vars = names(dt)[grepl('\\d{4}', names(dt))],
           value.name = 'weo_gdp', variable.name = 'year')
    setnames(dt, 'iso', 'country_code')

    # Convert year and GDP to numeric
    dt$year <- sub('x', '', dt$year) %>% as.numeric()
    dt$weo_gdp <- suppressWarnings(as.numeric(dt$weo_gdp))

    # Remove rows w/ missing GDP
    dt <- dt[!is.na(dt$weo_gdp)]

    # Remove current year and future years
    current_year <- format(Sys.Date(), '%Y')
    dt <- dt[dt$year < current_year]

    # Reshape to wide for GDP columns
    dt <- dt %>%
      dcast(formula = country_code + year ~ subject_code,
            value.var = 'weo_gdp')

    # ---- Merge with population ----

    pop <- pip_pop('load', maindir = maindir)
    setDT(pop)
    pop <- pop[pop_data_level == 'national', ]
    dt[pop,
       on = .(country_code, year),
       `:=`(
         pop = i.pop
       )
    ]

    # Calculate per capita value for NGDP_R
    dt[,
       weo_gdp_lcu := fifelse(
         is.na(weo_gdp_lcu), weo_gdp_lcu_notpc / pop, weo_gdp_lcu )
    ]


    # ---- Chain PPP and LCU GDP columns ----

    # Chain LCU on PPP column
    dt <- chain_values(
      dt, base_var = 'weo_gdp_ppp2017',
      replacement_var = 'weo_gdp_lcu',
      new_name = 'weo_gdp',
      by = 'country_code')


    # --- Sign and save ----

    # Select final columns
    dt <- dt[, c('country_code', 'year', 'weo_gdp')]

    # Save dataset
    pip_sign_save(x       = dt,
                  measure = measure,
                  msrdir  = msrdir,
                  force   = force)

  }  else if (action == "load") {

    dt <- load_aux(maindir = maindir,
                   measure = measure)
    return(dt)

  } else {
    rlang::abort(c("`action` must be `update` or `load`",
                   x = paste0("you provided `", action, "`")
    )
    )
  }

}
