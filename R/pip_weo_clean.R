#' Clean WEO data
#'
#' @param dt database with weo raw data
#' @param maindir directory where auxiliary data is stored (to load pop)
#' @param branch character: branch to be loaded
#'
#' @return data.table
#' @export
pip_weo_clean <- function(dt,
                          maindir = gls$PIP_DATA_DIR,
                          branch  = c("DEV", "PROD", "main")) {


  branch <- match.arg(branch)

#   ____________________________________________________________________________
#   Computations                                                            ####
  if (!inherits(dt, "data.table")) {
    setDT(dt)
  }

  # Clean column names
  nn <-
    names(dt) |>
    tolower() |>
    {\(.) gsub("[-/ ]", "_", .)}() |>
    {\(.) gsub("([0-9]{4})", "x\\1", .)}()

  names(dt) <- nn

  # ---- Data transformations ----

  # Select rows w/ data on real gdp per capita
  dt <- dt[weo_subject_code %in% c("NGDPRPC", "NGDPRPPPPC")]

  # Fix country codes
  dt[
    ,
    iso := fifelse(
      iso == "WBG", "PSE", iso # West Bank & Gaza
    )
  ][
    ,
    iso := fifelse(
      iso == "UVK", "XKX", iso # Kosovo
    )
  ][,
  # Replace subject codes
    subject_code := fcase(
     weo_subject_code == "NGDPRPC", "weo_gdp_lcu",
     weo_subject_code == "NGDPRPPPPC", "weo_gdp_ppp2017"
    )
  ]

  # Reshape to long format
  dt <-
    melt(data = dt,
      id.vars = c("iso", "subject_code"),
      measure.vars = names(dt)[grepl("\\d{4}", names(dt))],
      value.name = "weo_gdp", variable.name = "year"
    )
  setnames(dt, "iso", "country_code")

  # Convert year and GDP to numeric
  dt[,
     c("weo_gdp", "year") := {
       y <- sub("x", "", year) |>
         as.numeric()

       x <- as.numeric(weo_gdp) |>
         suppressWarnings()
       list(x, y)
     }]

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


  pop <- load_aux(measure = "pop",
                  maindir = maindir,
                  branch = branch)

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
    base_var        = "weo_gdp_ppp2017",
    replacement_var = "weo_gdp_lcu",
    new_name        = "weo_gdp",
    by              = "country_code"
  )


  # --- Sign and save ----

  # Select final columns
  dt <- dt[, c("country_code", "year", "weo_gdp")]



#   ____________________________________________________________________________
#   Return                                                                  ####
  return(dt)

}
