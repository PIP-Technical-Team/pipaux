#' Update POP
#'
#' @param from character: Source for population data.
#' @inheritParams pip_pop
pip_pop_update <-  function(force   = FALSE,
                            from    = c("gh", "file", "api"),
                            maindir = gls$PIP_DATA_DIR,
                            owner   = getOption("pipfun.ghowner"),
                            branch  = c("DEV", "PROD", "main"),
                            tag     = match.arg(branch)) {

  # Check arguments
  from    <- match.arg(from)
  branch <- match.arg(branch)
  measure <- "pop"

  # Get the most recent year in PFW to filter population projection

  pfw      <- pipload::pip_load_aux("pfw",
                                    branch  = branch,
                                    maindir = maindir)
  # year_max <- pfw[, max(year)]
  # get current year as max year
  year_max <- Sys.Date() |>
    format("%Y") |>
    as.numeric()
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # From WDI   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (from == "api") {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## from API --------

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

    ### Ger special cases ---------

    spop <- pipfun::load_from_gh(
      measure = measure,
      filename = "spop",
      owner  = owner,
      branch = branch,
      tag    = tag)  |>
      clean_names_from_wide() |>
      clean_from_wide()


    pop <- rbindlist(list(pop, spop),
            use.names = TRUE,
            fill = TRUE)


  } else {
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## from Emi's file --------

    # Now Emi's file is uploaded directly to GH. So we get it from there.
    # Load data

    pop_main <- pipfun::load_from_gh(
      measure = measure,
      owner  = owner,
      branch = branch,
      tag    = tag,
      ext    = "xlsx"
    ) |>
      clean_names_from_wide() |>
      clean_from_wide()


    ### Ger special cases ---------
    spop <- pipfun::load_from_gh(
      measure = measure,
      filename = "spop",
      owner  = owner,
      branch = branch,
      tag    = tag
    )  |>
      clean_names_from_wide() |>
      clean_from_wide()


    pop <- joyn::joyn(pop_main, spop,
                     by = c("country_code", "year", "pop_data_level"),
                     update_values = TRUE,
                     reportvar = FALSE,
                     verbose = FALSE)

    # pop <- rbindlist(list(pop_main, spop),
    #                       use.names = TRUE,
    #                       fill = TRUE)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Clean data   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Remove years prior to 1960
  pop <- pop[!is.na(pop) & year >= 1960]
  pop <- pop[year <= year_max]

  # sorting
  setorder(pop, country_code, year, pop_data_level)
  setcolorder(pop, c("country_code", "year", "pop_data_level", "pop"))

  pop[,
    pop_domain := fifelse(pop_data_level == 2, 1, 2)]

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


  # Remove any non-WDI countries
  cl <- load_aux(maindir = maindir,
                 measure = "country_list",
                 branch = branch)

  setDT(cl)
  pop <- pop[country_code %in% cl$country_code] |>
    unique() # make sure we don't havce any duplicates

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Save data   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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



#' Clean names from wide WDI format
#'
#' @param x data frame
#'
#' @return dataframe with names cleaned
#' @keywords internal
clean_names_from_wide <- function(x) {
  if (!is.data.table(x)) {
    setDT(x)
  }
  nnames <- as.character(x[2, 1:4])
  setnames(x, 1:4, nnames)
  x <- x[-c(1:2)]
  x
}


#' Clean from WDI format
#'
#' @param x data frame
#'
#' @return dataframe with names cleaned
#' @keywords internal
clean_from_wide <- function(x) {
  if (!is.data.table(x)) {
    setDT(x)
  }


  year_vars            <- names(x)[6:ncol(x)]
  x$Series_Name <- NULL
  x$Time_Name   <- NULL

  # Reshape to long format
  pop_long <- x |>
    data.table::setDT() |>
    data.table::melt(
      id.vars = c("Country", "Series"),
      measure.vars = year_vars,
      variable.name = "Year",
      value.name = "Population"
    )
  pop_long[,
           Year := as.numeric(as.character(Year))
           ][,
             Population := {
               Population[Population == "."] <- NA_character_
               as.numeric(Population)
             }]



  pop <- pop_long
  # Create data_level column
  pop[,
      pop_data_level :=
        fcase(
          grepl("POP", Series), 2,
          grepl("RUR", Series), 0,
          grepl("URB", Series), 1
        )
  ][,
    Series := NULL]

  # Set colnames
  setnames(
    pop,
    old = c("Country", "Year", "Population"),
    new = c("country_code", "year", "pop")
  )

  return(pop)
}
