#' Create table with missing countries
#'
#' @inheritParams pip_cpi
#' @inheritParams pipfun::load_from_gh
#'
#' @return if `action = "update"` returns logical. If `action = "load"` returns
#'   a data.table
#' @export
pip_missing_data <- function(action  = c("update", "load"),
                             force   = FALSE,
                             owner   = getOption("pipfun.ghowner"),
                             maindir = gls$PIP_DATA_DIR,
                             branch  = c("DEV", "PROD", "main"),
                             tag     = match.arg(branch)
                             ) {

  measure <- "missing_data"
  branch <- match.arg(branch)
  action <- match.arg(action)

#   ________________________________________________________________
#   Computations                                              ####


  if (action == "update") {

    # Load data ------

    # pipload::pip_load_all_aux(aux = c("gdp", "pce", "pfw", "country_list"),
    #                           aux_names = c("gdp", "pce", "pfw", "cl"),
    #                           replace = TRUE)

    gdp <- load_aux(maindir = maindir,
                    branch  = branch,
                    measure = "gdp")

    pce <- load_aux(maindir = maindir,
                    branch  = branch,
                    measure = "pce")

    pfw <- load_aux(maindir = maindir,
                    branch  = branch,
                    measure = "pfw")

    pop <- load_aux(maindir = maindir,
                    branch  = branch,
                    measure = "pop")

    cl <- load_aux(maindir = maindir,
                   branch  = branch,
                    measure = "country_list")


    ref_years <- gls$PIP_REF_YEARS

    # Prepare NAC data ------

    # Standardize ^_data_level ^_domain column names
    setnames(pce, names(pce) ,sub("^pce[_]", "nac_", names(pce)))
    setnames(gdp, names(gdp) ,sub("^gdp[_]", "nac_", names(gdp)))

    nac <- joyn::joyn(gdp, pce,
                       by = c(
                         "country_code", "year", "nac_data_level",
                         "nac_domain"),
                       match_type = "1:1",
                       reportvar = FALSE,
                       verbose = FALSE,
                      keep = "full")

    nac <-
      nac[year %in% ref_years
      ][,
        nac_domain := NULL]



    # Prepare Grid of all countries and ref years ---------

    cl <- cl[, c("country_code", "region_code")]

    gr <-
      expand.grid(country_code = cl$country_code,
                  year = ref_years,
                  stringsAsFactors = FALSE) |>
      as.data.table()

    setorder(gr, country_code, year)

    # Prepare PFW data -------

    # domvar <- grep("domain$", names(pfw), value = TRUE) # all domains
    domvar <- grep("(gdp|pce)_domain$", names(pfw), value = TRUE) # data_level vars
    idvars <- c("country_code", "year", "welfare_type")
    keepv  <- c(idvars, domvar)
    dom <- pfw[inpovcal == 1
    ][, ..keepv]

    # pfw[, mdom := max(.SD[, mget(domvar)]), by = .I]  # when 1.14.3 becomes available
    dom[,
        #  Create row position (must be done in 1.14.2)
        n := .I
    ][,
      # max reporting year per country/year
      mdom := max(.SD[, mget(domvar)]), by = n
    ]

    dom <- dom[,
               .(country_code, year, welfare_type, mdom)]

    ## National data ------------
    domn <- dom[mdom == 1]

    domn <- domn[,
                 nac_data_level := "national"]

    ## Urban/Rural data -----------
    domu <-  dom[mdom == 2]
    domr <-  dom[mdom == 2]

    domu <- domu[,
                 nac_data_level := "urban"]

    domr <- domr[,
                 nac_data_level := "rural"]

    ## append national and urban/rural sections ----

    dom2 <-
      list(domn, domu, domr)  |>
      rbindlist()

    dom2 <- unique(dom2[, c("country_code", "year", "nac_data_level")])


    ## Survey availability -----------
    svav <- unique(pfw[, .(country_code, year)])

    ctr_with_survey <- svav[, unique(country_code)] # countries with survey

    # Find countries/years with no survey ------
    ct_nosv <- gr[!country_code %in% ctr_with_survey]

    # Find countries with no GDP or PCE ------------------
    ct_sv <- gr[country_code %in% ctr_with_survey] # countries with survey

    # Join grid of countries with at least one survey in to the NAC data. Those
    # in the grid that are not in the NAC should be considered as missind data
    # as well Those that are in NAC but are not ingrid are those without survey,
    # which we took care above

    ct_nonac <- joyn::joyn(ct_sv, nac,
                            by = c("country_code", "year"),
                            match_type = "1:m",
                            verbose = FALSE)


    ct_nonac <-
      ct_nonac[.joyn == "x"
               ][,
                 c("country_code", "year")
                 ]


    # Get Countries with missing data ---------

    ct_miss_data <-
      list(ct_nosv, ct_nonac) |>
      rbindlist(use.names = TRUE)


    setorder(ct_miss_data, country_code, year)


# Add region_code -------
  ct_miss_data <- joyn::joyn(ct_miss_data, cl,
                              by = "country_code",
                              keep = "left",
                              match_type = "m:1",
                              reportvar = FALSE,
                              verbose = FALSE)


# Join  with pop data -----
    pop <-
      pop[pop_data_level == "national"
      ][,
        c("pop_domain", "pop_data_level") := NULL
      ]

    setnames(pop, "pop", "reporting_pop")
    # Filter pop with missing data countries
    pop_md <-
      pop[ct_miss_data,
          on = c("country_code", "year")]

#  .................................................................
##  Save data                                                    ####
    msrdir  <- fs::path(maindir, "_aux/", branch, measure)

    saved <- pipfun::pip_sign_save(
      x       = pop_md,
      measure = measure,
      msrdir  = msrdir,
      force   = force
    )

    return(invisible(saved))

  } else {
    dl <- load_aux(
      maindir = maindir,
      measure = measure,
      branch  = branch
    )
    return(dl)
  }

} # end of function pip_missing_data
