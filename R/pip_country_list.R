#' List of countries
#'
#' Load or update dataset with WDI countries. See details.
#'
#' This function creates a combined dataset of countries in WDI and their
#' respective regional classification by querying `wbstats::wb_countries()`, as
#' well as reading from the PovcalNet Masterfile to fetch PCN region codes.
#'
#' The dependency on the PCN Masterfile should be changed in the future.
#'
#' @inheritParams pip_prices
#' @param pcndir character: PovcalNet directory for the Masterfile.
#' @export
pip_country_list <- function(action = "update",
                             force = FALSE,
                             pcndir = getOption("pipaux.pcndir"),
                             maindir = getOption("pipaux.maindir")) {
  measure <- "country_list"
  msrdir <- paste0(maindir, "_aux/", measure, "/") # measure dir

  if (action == "update") {

    # ---- Read PCN Masterfile ----

    # Get list of files
    m_files <- list.files(
      pcndir,
      pattern = "Master_2021[0-9]{10}.xlsx"
    )

    # Find latest Masterfile
    pcn_master_path <- m_files %>%
      gsub("Master_|.xlsx", "", .) %>%
      as.POSIXlt(format = "%Y%m%d%H%M%S") %>%
      max(na.rm = TRUE) %>%
      as.character() %>%
      gsub("-|:| ", "", .) %>%
      sprintf("%s/Master_%s.xlsx", pcndir, .)

    # Read CountryList sheet from latest Masterfile
    df <- readxl::read_xlsx(pcn_master_path, sheet = "CountryList")
    df <- data.table::setDT(df)
    df <- df[, c("CountryCode", "WBRegionCode")]
    names(df) <- c("country_code", "pcn_region_code")


    # ---- Read country data from WDI ----

    country_list <- wbstats::wb_countries()
    data.table::setDT(country_list)
    country_list <- country_list[region != "Aggregates"]

    # Recode names
    setnames(country_list,
      old = c("iso3c", "region_iso3c", "admin_region_iso3c"),
      new = c("country_code", "region_code", "admin_region_code")
    )

    # Merge w/ PCN data
    country_list <- data.table::merge.data.table(
      country_list, df,
      all.x = TRUE, by = "country_code"
    )

    # Keep relevant variables
    country_list <-
      country_list[
        ,
        .(
          country_code, country, region, region_code,
          admin_region_code, pcn_region_code,
          income_level, lending_type
        )
      ]

    # ---- Save to file ----

    pip_sign_save(
      x = country_list,
      measure = "country_list",
      msrdir = msrdir,
      force = force
    )
  } else if (action == "load") {
    df <- load_aux(
      maindir = maindir,
      measure = measure
    )
    return(df)
  } else {
    msg <- paste("action `", action, "` is not a valid action.")
    rlang::abort(c(
      msg,
      i = "make sure you selected `update` or `load`"
    ),
    class = "pipaux_error"
    )
  }
}
