#' Fetch GDP data from WEO
#'
#' Create a dataset with GDP data from World Economic Outlook.
#'
#' Note that the most recent version most be downloaded from imf.org and saved
#' as an .xls file in `<maindir>/_aux/weo/`. The filename should be in the
#' following structure `WEO_<YYYY-DD-MM>.xls`. Due to potential file corruption
#' the file must be opened and re-saved before it can be updated with
#' `pip_weo()`. Hopefully in the future IMF will stop using an `.xls` file
#' that's not really xls.
#'
#' @inheritParams pip_prices
#' @export
pip_weo <- function(action  = c("update", "load"),
                    force   = FALSE,
                    owner   = getOption("pipaux.ghowner"),
                    maindir = gls$PIP_DATA_DIR,
                    branch  = c("DEV", "PROD", "main"),
                    tag     = match.arg(branch)) {
  measure <- "weo"
  branch <- match.arg(branch)

  if (action == "update") {

    # ---- Load data from disk ----

    # Read data
    dt <- load_raw_aux(
      measure = measure,
      owner  = owner,
      branch = branch,
      tag    = tag
    )
    return(dt)

    dt <- readxl::read_xls(
      weo_latest,
      sheet = 1, na = "n/a",
      col_types = "text"
    )
    dt <- setDT(dt)

    # Clean column names
    dt <- janitor::clean_names(dt)

    # ---- Data transformations ----

    # Select rows w/ data on real gdp per capita
    dt <- dt[weo_subject_code %in% c("NGDPRPC", "NGDPRPPPPC")]

    # Fix country codes
    dt[
      ,
      iso := fifelse(
        iso == "WBG", "PSE", iso # West Bank & Gaza
      )
    ]
    dt[
      ,
      iso := fifelse(
        iso == "UVK", "XKX", iso # Kosovo
      )
    ]

    # Replace subject codes
    dt[,
      subject_code := fcase(
        weo_subject_code == "NGDPRPC", "weo_gdp_lcu",
        weo_subject_code == "NGDPRPPPPC", "weo_gdp_ppp2017"
      )
    ]

    # Reshape to long format
    dt <- dt %>%
      melt(
        id.vars = c("iso", "subject_code"),
        measure.vars = names(dt)[grepl("\\d{4}", names(dt))],
        value.name = "weo_gdp", variable.name = "year"
      )
    setnames(dt, "iso", "country_code")

    # Convert year and GDP to numeric
    dt[,
        year := {
          x <- sub("x", "", year)
          as.numeric(x)
        }]

    dt[,
       weo_gdp := suppressWarnings(as.numeric(weo_gdp))]

    # Remove rows w/ missing GDP`
    dt <- na.omit(dt, cols = "weo_gdp")

    # Remove current year and future years
    current_year <- format(Sys.Date(), "%Y")
    dt <- dt[year < current_year]

    # Reshape to wide for GDP columns
    dt <- dcast(dt,
        formula = country_code + year ~ subject_code,
        value.var = "weo_gdp"
      )

    # ---- Merge with population ----

    pop <- pip_pop("load", maindir = maindir)
    setDT(pop)
    pop <- pop[pop_data_level == "national", ]
    dt[pop,
      on = .(country_code, year),
      `:=`(
        pop = i.pop
      )
    ]

    # ---- Chain PPP and LCU GDP columns ----

    # Chain LCU on PPP column
    dt <- chain_values(
      dt,
      base_var = "weo_gdp_ppp2017",
      replacement_var = "weo_gdp_lcu",
      new_name = "weo_gdp",
      by = "country_code"
    )


    # --- Sign and save ----

    # Select final columns
    dt <- dt[, c("country_code", "year", "weo_gdp")]

    # Save dataset
    msrdir <- fs::path(maindir, "_aux", branch, measure) # measure dir
    pip_sign_save(
      x = dt,
      measure = measure,
      msrdir = msrdir,
      force = force
    )
  } else if (action == "load") {
    dt <- load_aux(
      maindir = maindir,
      measure = measure
    )
    return(dt)
  } else {
    rlang::abort(c("`action` must be `update` or `load`",
      x = paste0("you provided `", action, "`")
    ))
  }
}
