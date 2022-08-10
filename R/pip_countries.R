#' PIP Countries
#'
#' Update or load a dataset with countries.
#'
#' @inheritParams pip_prices
#' @inheritParams pipfun::load_from_gh
#' @export
pip_countries <- function(action = c("update", "load"),
                          force = FALSE,
                          maindir = gls$PIP_DATA_DIR,
                          owner   = getOption("pipfun.ghowner"),
                          branch  = c("DEV", "PROD", "main"),
                          tag     = match.arg(branch)
                          ) {

  measure <- "countries"
  action <- match.arg(action)
  msrdir <- fs::path(maindir, "_aux/", measure)

  if (action == "update") {

    ## Special national accounts --------
    cl <- pipfun::load_from_gh(
      measure = "country_list",
      owner  = owner,
      branch = branch,
      tag    = tag
    )

    pfw <- load_aux("pfw", maindir = maindir)


    pfw <- pfw[inpovcal == 1,
               ][,
                 c("country_code")
                 ] |>
      unique()


    countries <- cl[country_code %in% pfw$country_code, ]

    pip_sign_save(
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
