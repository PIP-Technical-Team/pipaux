pick_latest_weo_file <- function(dir, pattern = "WEO_.*[.]xls") {

  # Get latest version of file (in case there are more)
  files <- list.files(dir, pattern = pattern)
  out <- files %>%
    gsub("WEO_|.xls", "", .) %>%
    as.POSIXlt() %>%
    max() %>%
    as.character() %>%
    sprintf("WEO_%s.xls", dir, .)

  return(out)
}

#' Load raw WEO GDP
#'
#' @param path character: Path
#' @return list
#' @keywords internal
load_weo_gdp <- function(path) {
  assertthat::assert_that(tools::file_ext(path) == "xls",
                          msg = "File extention must be .xls")
  df <- readxl::read_xls(
    path, sheet = 1, na = "n/a", col_types = "text")
  return(df)
}

#' Transform WEO GDP
#'
#' @param df data.frame: WEO GDP data.
#' @param pop data.frame: Population data.
#'
#' @return data.table
#' @keywords internal
transform_weo_gdp <- function(df, pop) {

  # Convert to data.table
  df <- data.table::setDT(dt)

  # Clean column names
  dt <- janitor::clean_names(dt)

  # ---- Data transformations ----

  # Select rows w/ data on real gdp per capita
  dt <- dt[weo_subject_code %in%
             c("NGDPRPC", "NGDPRPPPPC", "NGDP_R")]

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
  dt[
    ,
    subject_code := fcase(
      weo_subject_code == "NGDPRPC", "weo_gdp_lcu",
      weo_subject_code == "NGDPRPPPPC", "weo_gdp_ppp2017",
      weo_subject_code == "NGDP_R", "weo_gdp_lcu_notpc"
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
  dt$year <- sub("x", "", dt$year) %>% as.numeric()
  dt$weo_gdp <- suppressWarnings(as.numeric(dt$weo_gdp))

  # Remove rows w/ missing GDP
  dt <- dt[!is.na(dt$weo_gdp)]

  # Remove current year and future years
  current_year <- format(Sys.Date(), "%Y")
  dt <- dt[dt$year < current_year]

  # Reshape to wide for GDP columns
  dt <- dt %>%
    dcast(
      formula = country_code + year ~ subject_code,
      value.var = "weo_gdp"
    )

  # ---- Merge with population ----

  setDT(pop)
  pop <- pop[pop_data_level == "national", ]
  dt[pop,
     on = .(country_code, year),
     `:=`(
       pop = i.pop
     )
  ]

  # Calculate per capita value for NGDP_R
  dt[
    ,
    weo_gdp_lcu := fifelse(
      is.na(weo_gdp_lcu), weo_gdp_lcu_notpc / pop, weo_gdp_lcu
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


  # --- Finalize table ----

  # Select final columns
  dt <- dt[, c("country_code", "year", "weo_gdp")]

  return(dt)

}

#' Verify input WEO GDP
#'
#' @inheritParams transform_weo_gdp
#' @return data.frame
#' @keywords internal
verify_input_weo_gdp <- function(df){

}

#' Verify output WEO GDP
#'
#' @inheritParams transform_weo_gdp
#' @return data.frame
#' @keywords internal
verify_output_weo_gdp <- function(df){

}
