#' @importFrom magrittr %>%
NULL

#' Update POP
#'
#' @param src character: Source for population data.
#' @inheritParams pip_prices
#' @keywords internal
#' @import data.table
pip_pop_update <-  function(force   = FALSE,
                            src     = c("emi", "wdi"),
                            maindir = gls$PIP_DATA_DIR) {

    cl <- pip_country_list("load", maindir = maindir)

  # Check arguments
  src <- match.arg(src)

  # Directories
  measure <- "pop"
  msrdir  <- fs::path(maindir, "_aux/", measure)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# From WDI   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (src == "wdi") {
    pop <- purrr::map_df(codes, ~ {
      df <- wbstats::wb_data(indicator = .x, lang = "en")
      colnames(df)[colnames(df) == .x] <- "pop"
      df$coverage <- .x
      return(df)

    })

    setDT(pop)

    # data level
    pop[
      ,
      pop_data_level :=
        fcase(
          grepl("POP", coverage), 2,
          grepl("RUR", coverage), 0,
          grepl("URB", coverage), 1
        )
    ]
    setnames(pop,
      old = c("iso3c", "date"),
      new = c("country_code", "year")
    )
  } else if (src == "emi") {

    # Load main data file from Emi

    # compare data between PCN and PIP
    pcn_pop_dir <- "P:/01.PovcalNet/03.QA/03.Population/data/"

    if (dir.exists(pcn_pop_dir)) {
      pcn_pop_files   <- list.files(pcn_pop_dir,
                                    pattern = "population.*\\.xlsx")
    } else {
      pcn_pop_files <- NULL
    }

    pip_pop_dir   <- fs::path(msrdir, "raw_data/")
    pip_pop_files <- list.files(pip_pop_dir)

    miss_pop_files <- pcn_pop_files[!(pcn_pop_files %in% pip_pop_files)]

    if (length(miss_pop_files) != 0) {

      cli::cli_process_start("Copying POP files from PCN folder to PIP folder")

      file.copy(from      = paste0(pcn_pop_dir, miss_pop_files),
                to        = paste0(pip_pop_dir, miss_pop_files),
                overwrite = FALSE)

      cli::cli_process_done()
    }

    # find only population_country files
    pip_pop_files <- list.files(pip_pop_dir,
                                pattern = "population_country.*\\.xlsx")

    # Get latest version of file
    pop_latest <- pip_pop_files %>%
      gsub("population_country_|.xlsx", "", .) %>%
      as.POSIXlt() %>%
      max() %>%
      as.character() %>%
      sprintf("population_country_%s.xlsx", .)

    pop_path <- paste0(pip_pop_dir, pop_latest)

    # Load data
    pop_main <- suppressMessages(
      readxl::read_xlsx(pop_path, sheet = "Sheet1")
    )
    names(pop_main)[1:4] <- as.character(pop_main[2, 1:4])
    pop_main             <- pop_main[-c(1:2), ]
    year_vars            <- names(pop_main[, 6:ncol(pop_main)])
    pop_main$Series_Name <- NULL
    pop_main$Time_Name   <- NULL

    # Reshape to long format
    pop_long <- pop_main %>%
      data.table::setDT() %>%
      data.table::melt(
        id.vars = c("Country", "Series"),
        measure.vars = year_vars,
        variable.name = "Year",
        value.name = "Population"
      )
    pop_long$Year <- as.character(pop_long$Year)
    pop_long$Population <- as.numeric(pop_long$Population)

    # Merge with special country data
    # Note for PSE, KWT and SXM, some years of population data are missing in Emi's
    # main file and hence in WDI. Here we are complementing the main file with an
    # additional file she shared to assure complete coverage. This file contains
    # historical data and will not need to be updated every year. Hence, here we are
    # just calling the version we received. Should we receive a new version,
    # the import line below should be updated to reflect the accurate file.

    # Load data
    pop_special_file <-
      list.files(pip_pop_dir,
                 pattern = "population_missing.*\\.xlsx") %>%
      gsub("population_missing_|.xlsx", "", .) %>%
      as.POSIXlt() %>%
      max() %>%
      as.character() %>%
      sprintf("population_missing_%s.xlsx", .)

    pop_special_path        <- paste0(pip_pop_dir, pop_special_file)
    pop_special             <- suppressMessages(readxl::read_xlsx(pop_special_path,
                                                                  sheet = "Long")
                                                )
    pop_special             <- pop_special[c("Country", "Series", "Time", "Data")]
    names(pop_special)[3:4] <- c("Year", "Population")
    pop_special$Year        <- sub("YR", "", pop_special$Year)

    # Merge datasets
    pop_merge      <- rbind(pop_long, pop_special)
    pop_merge$Year <- as.numeric(pop_merge$Year)
    data.table::setDT(pop_merge)

    # Create data_level column
    pop_merge[
      ,
      pop_data_level :=
        fcase(
          grepl("POP", Series), 2,
          grepl("RUR", Series), 0,
          grepl("URB", Series), 1
        )
    ]

    # Set colnames
    setnames(
      pop_merge,
      old = c("Country", "Year", "Population"),
      new = c("country_code", "year", "pop")
    )
    pop_merge$Series <- NULL
    pop <- pop_merge

    # Remove years prior to 1960
    pop <- pop[year >= 1960]

    # Remove years after 2020
    pop <- pop[year <= 2020]

    # Remove rows w/ missing pop values
    pop <- pop[!is.na(pop)]
  } else {
    msg <- paste("src `", src, "` is not a valid source.")
    rlang::abort(c(
      msg,
      i = "make sure you select `wdi` or `emi`"
    ),
    class = "pipaux_error"
    )
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Clean data   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  pop <- pop[, c("country_code", "year", "pop_data_level", "pop")
  ][,
    pop_domain := fifelse(pop_data_level == 2, 1, 2)
  ]

  # recode domain and data_level variables
  cols <- c("pop_domain", "pop_data_level")
  pop[,
    (cols) := lapply(.SD, as.character),
    .SDcols = cols
  ][
    , # recode domain
    pop_domain := fcase(
      pop_domain == "1", "national",
      pop_domain == "2", "urban/rural",
      pop_domain == "3", "subnational region"
    )
  ][ # Recode data_level only for those that are national or urban/rural
    pop_domain %in% c("national", "urban/rural"),
    pop_data_level := fcase(
      pop_data_level == "0", "rural",
      pop_data_level == "1", "urban",
      pop_data_level == "2", "national"
    )
  ]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save data   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Remove any non-WDI countries
  cl <- pip_country_list("load", maindir = maindir)
  setDT(cl)
  pop <- pop[country_code %in% cl$country_code]

  pip_sign_save(x       = pop,
                measure = "pop",
                msrdir  = msrdir,
                force   = force)
}
