#' @importFrom magrittr %>%
NULL

#' Update POP
#'
#' @param src character: Source for population data.
#' @inheritParams pip_cpi
#' @keywords internal
#' @import data.table
pip_pop_update <-  function(force   = FALSE,
                            src     = c("emi", "wdi"),
                            maindir = gls$PIP_DATA_DIR,
                            owner   = getOption("pipfun.ghowner"),
                            branch  = c("DEV", "PROD", "main"),
                            tag     = match.arg(branch)) {

  # Check arguments
  src    <- match.arg(src)
  branch <- match.arg(branch)

  # Directories
  measure <- "pop"

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
  } else {


    # Now Emi's file is uploaded directly to GH. So we get it from there.

    # Load data

    pop_main <- pipfun::load_from_gh(
      measure = measure,
      owner  = owner,
      branch = branch,
      tag    = tag,
      ext    = "xlsx"
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
    pop_long$Year <- as.numeric(as.character(pop_long$Year))
    pop_long$Population <- as.numeric(pop_long$Population)

    # Merge with special country data
    # Note for PSE, KWT and SXM, some years of population data are missing in Emi's
    # main file and hence in WDI. Here we are complementing the main file with an
    # additional file she shared to assure complete coverage. This file contains
    # historical data and will not need to be updated every year. Hence, here we are
    # just calling the version we received. Should we receive a new version,
    # the import line below should be updated to reflect the accurate file.

    # # Load data
    # pop_special_file <-
    #   list.files(pip_pop_dir,
    #              pattern = "population_missing.*\\.xlsx") %>%
    #   gsub("population_missing_|.xlsx", "", .) %>%
    #   as.POSIXlt() %>%
    #   max() %>%
    #   as.character() %>%
    #   sprintf("population_missing_%s.xlsx", .)
    #
    # pop_special_path        <- fs::path(pip_pop_dir, pop_special_file)
    # pop_special             <- suppressMessages(readxl::read_xlsx(pop_special_path,
    #                                                               sheet = "Long")
    #                                             )
    # pop_special             <- pop_special[c("Country", "Series", "Time", "Data")]
    # names(pop_special)[3:4] <- c("Year", "Population")
    # pop_special$Year        <- sub("YR", "", pop_special$Year)
    #
    # # Merge datasets
    # pop_merge      <- rbind(pop_long, pop_special)
    # pop_merge$Year <- as.numeric(pop_merge$Year)
    # data.table::setDT(pop_merge)
    #
    # pop <- pop_merge

    pop <- pop_long
    # Create data_level column
    pop[
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
      pop,
      old = c("Country", "Year", "Population"),
      new = c("country_code", "year", "pop")
    )
    pop$Series <- NULL

    # Remove years prior to 1960
    pop <- pop[year >= 1960]

    # Remove years after 2020
    #pop <- pop[year <= 2020]

    # Remove rows w/ missing pop values
    pop <- pop[!is.na(pop)]

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
  # Remove any non-WDI countries
  cl <- load_aux(maindir = maindir,
                 measure = "country_list",
                 branch = branch)

  setDT(cl)
  pop <- pop[country_code %in% cl$country_code]

  # Save
  if (branch == "main") {
    branch <- ""
  }
  msrdir <- fs::path(maindir, "_aux", branch, measure) # measure dir
  saved <- pipfun::pip_sign_save(
    x       = pop,
    measure = measure,
    msrdir  = msrdir,
    force   = force
  )

  return(invisible(saved))

}
