#' PIP Countries
#'
#' Update or load a dataset with countries.
#'
#' @inheritParams pip_cpi
#' @inheritParams pipfun::load_from_gh
#' @export
pip_countries <- function(action  = c("update", "load"),
                          force   = FALSE,
                          owner   = getOption("pipfun.ghowner"),
                          maindir = gls$PIP_DATA_DIR,
                          branch  = c("DEV", "PROD", "main"),
                          tag     = match.arg(branch)) {

  measure <- "countries"
  action <- match.arg(action)
  branch <- match.arg(branch)

  if (action == "update") {

    ## Special national accounts --------
    cl <- pipfun::load_from_gh(
      measure = "country_list",
      owner  = owner,
      branch = branch,
      tag    = tag
    )

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
