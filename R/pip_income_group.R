#' PIP series of income group
#'
#' Update or load a dataset with countries.
#'
#' @inheritParams pip_cpi
#' @inheritParams pipfun::load_from_gh
#' @export
pip_income_group <- function(action  = c("update", "load"),
                          force   = FALSE,
                          owner   = getOption("pipfun.ghowner"),
                          maindir = gls$PIP_DATA_DIR,
                          branch  = c("DEV", "PROD", "main"),
                          tag     = match.arg(branch)) {

  measure <- "income_group"
  action <- match.arg(action)
  branch <- match.arg(branch)

  if (action == "update") {

    ## Special national accounts --------
    ig <- pipfun::load_from_gh(
      measure  = measure,
      owner    = "GPID-WB",
      repo     = "Class",
      branch   = "master",
      filename = "OutputData/CLASS",
      ext      = "dta"
    ) |>
      fselect(code, year_data, incgroup_historical)

    # rename variables
    setnames(x = ig,
             old = c("code", "year_data", "incgroup_historical"),
             new = c("country_code", "year", "income_group"))

    ig[,
       income_group_code := fcase(income_group == "High income", "HIC",
                                  income_group == "Upper middle income", "UMIC",
                                  income_group == "Lower middle income", "LMIC",
                                  income_group == "Low income", "LIC",
                                  default = "")]


    # Remove any non-WDI countries
    cl <- load_aux(maindir = maindir,
                   measure = "country_list",
                   branch = branch)

    ig <- ig[country_code %in% cl$country_code]

    pfw <- load_aux(measure = "pfw",
                    maindir = maindir,
                    branch  = branch)


    pfw <- pfw[inpovcal == 1,
    ][,
      c("country_code")
    ] |>
      unique()


    countries <- cl[country_code %in% pfw$country_code
    ][,
      c("pcn_region", "pcn_region_code") := NULL]

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## save --------

    if (branch == "main") {
      branch <- ""
    }
    msrdir <- fs::path(maindir, "_aux", branch, measure) # measure dir


    pipfun::pip_sign_save(
      x = countries,
      measure = measure,
      msrdir = msrdir,
      force = force
    )
  } else {
    df <- load_aux(
      maindir = maindir,
      measure = measure
    )
    return(df)
  }
}
