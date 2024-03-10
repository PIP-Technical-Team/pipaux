#' National Poverty headcount
#'
#' Update series of national poverty lines
#'
#' @inheritParams pip_cpi
#' @inheritParams pipfun::load_from_gh
#' @export
pip_npl <- function(action  = c("update", "load"),
                   force   = FALSE,
                   owner   = getOption("pipfun.ghowner"),
                   maindir = gls$PIP_DATA_DIR,
                   branch  = c("DEV", "PROD", "main"),
                   tag     = match.arg(branch)) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## setup --------

  measure <- "npl"
  branch <- match.arg(branch)
  action <- match.arg(action)

  if (action == "update") {
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## update --------

    npl <- pipfun::load_from_gh(measure = measure,
                                owner  = owner,
                                branch = branch,
                                tag    = tag,
                                ext    = "dta") |>
      setDT()

    # validate npl raw data
    npl_validate_raw(npl)

    setnames(x = npl,
             old = c("countrycode",  "year", "vsi_pov_nahc_nc"),
             new = c("country_code", "reporting_year", "nat_headcount"),
             skip_absent = TRUE)

    npl[, c("region", "vsi_pov_nahc") := NULL]
    npl[, nat_headcount := nat_headcount / 100]


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## save --------

    # validate npl output data
    npl_validate_output(npl)

    if (branch == "main") {
      branch <- ""
    }
    msrdir <- fs::path(maindir, "_aux", branch, measure) # measure dir

    saved <- pipfun::pip_sign_save(
      x       = npl,
      measure = measure,
      msrdir  = msrdir,
      force   = force
    )


  } else {
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## load --------

    load_aux(
      maindir = maindir,
      measure = measure,
      branch  = branch
    )

  }
}
