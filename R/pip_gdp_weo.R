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
#' @return data.table
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
      sprintf("%sWEO_%s.xls", maindir, . )

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
    dt$weo_gdp <- as.numeric(dt$weo_gdp)

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

    # Add column for countries where the entire PPP series is missing
    all_ppp_na <- dt[, .(all_ppp_na = all(is.na(weo_gdp_ppp2017))),
                     by = country_code]
    dt[all_ppp_na,
       on = .(country_code),
       `:=`(
         all_ppp_na = i.all_ppp_na
       )
    ]

    # Create new GDP variable
    dt[,
        weo_gdp := fifelse(all_ppp_na, weo_gdp_lcu, weo_gdp_ppp2017)
    ][,
      # Lagged and lead values of new GDP
      `:=`(
        weo_gdp_lag  = shift(weo_gdp),
        weo_gdp_lead = shift(weo_gdp, type = "lead")
      ),
      by = .(country_code)
    ][
      , # Row ID by country
      n := rowid(country_code)
    ]

    # Linking factors
    dt[,
        `:=`(
          # linking factors back
          bck = (!is.na(weo_gdp)
                 & !is.na(weo_gdp_lag)
                 & n != 1) * (weo_gdp / weo_gdp_lcu),

          # linking factors forward
          fwd = (!is.na(weo_gdp)
                 & !is.na(weo_gdp_lead)
                 & n != .N) * (weo_gdp / weo_gdp_lcu)
        ),
        by = .(country_code)
    ][,
      `:=`(
        # Max value in linking factor
        bcki = max(bck, na.rm = TRUE),
        fwdi = max(fwd, na.rm = TRUE)
      ),
      by = .(country_code)
    ]

    # Apply: create linked value
    dt[,
        `:=`(
          vbck = weo_gdp_lcu * bcki,
          vfwd = weo_gdp_lcu * fwdi
        )
    ]

    # Assess where to apply forward of backward
    dt[,
        gapsum := {
          gap     <- (is.na(weo_gdp) & !is.na(weo_gdp_lag))
          gap     <- fifelse(n == 1, TRUE, gap)
          gapsum  <-  sum(gap)
        },
        by = .(country_code)
    ]

    # Replace where missing and indicate source
    dt[
      ,
      dt := {
        # fwd
        weo_gdp = fifelse(is.na(weo_gdp) & gapsum == 2, vfwd, weo_gdp)
        # bck
        weo_gdp = fifelse(is.na(weo_gdp) & gapsum == 1, vbck, weo_gdp)
      }
    ]

    # --- Sign and save ----

    # Select final columns
    dt <- dt[, c('country_code', 'year', 'weo_gdp')]

    # Save dataset
    pip_sign_save(x       = dt,
                  measure = measure,
                  msrdir  = msrdir,
                  force   = force)

  }  else if (action == "load") {

    dt <- load_aux(msrdir  = msrdir,
                   measure = measure)
    return(dt)

  } else {
    rlang::abort(c("`action` must be `update` or `load`",
                   x = paste0("you provided `", action, "`")
    )
    )
  }

}
