#' @importFrom magrittr %>%
NULL

#' Update POP
#'
#' @param from character: Source for population data.
#' @inheritParams pip_cpi
#' @keywords internal
#' @import data.table
pip_pop_update <-  function(force   = FALSE,
                            from    = c("gh", "file", "api"),
                            maindir = gls$PIP_DATA_DIR,
                            owner   = getOption("pipfun.ghowner"),
                            branch  = c("DEV", "PROD", "main"),
                            tag     = match.arg(branch)) {

  # Check arguments
  from    <- match.arg(from)
  branch <- match.arg(branch)

  # Directories
  measure <- "pop"

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # From WDI   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (from == "api") {

    pop_indicators <- c("SP.POP.TOTL", "SP.RUR.TOTL", "SP.URB.TOTL")
    pop   <- wbstats::wb_data(indicator = pop_indicators,
                              country = "all", # this is new
                              lang      = "en",
                              return_wide = FALSE) |>
      setDT()



    # rename vars
    pop <- pop[, c("iso3c", "date", "indicator_id", "value")]

    setnames(pop,
             new = c("country_code", "year", "coverage", "pop"))



    pop[,
        year := as.numeric(year)
    ][,
      pop_data_level :=
        fcase(
          grepl("POP", coverage), 2,
          grepl("RUR", coverage), 0,
          grepl("URB", coverage), 1
        )
    ][,
      coverage := NULL]

    pop <- pop[!is.na(pop)]
    setorder(pop, country_code, year, pop_data_level)
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
