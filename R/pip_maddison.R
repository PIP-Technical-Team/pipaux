#' Maddison Project Data
#'
#' @inheritParams pip_prices
#' @export
#' @import data.table
pip_maddison <- function(action = "update",
                         force  = FALSE,
                         maindir = getOption("pipaux.maindir")) {

  measure <- "maddison"
  msrdir  <- paste0(maindir, "_aux/", measure, "/") # measure dir

  if (action == "update") {

    mpd <- haven::read_dta(getOption("pipaux.madsrc"))
    mpd <- mpd[mpd$year >= 1960, ] # Historical data is not needed

    setDT(mpd)
    setnames(mpd,
             old = c("countrycode", "country", "gdppc"),
             new = c("country_code", "country_name", "mpd_gdp")
             )

    pip_sign_save(x       = mpd,
                  measure = measure,
                  msrdir  = msrdir,
                  force   = force)


  } else if (action == "load") {

    df <- load_aux(msrdir  = msrdir,
                       measure = measure)
    return(df)

  } else {
    rlang::abort(c("`action` must be `update` or `load`",
                 x = paste0("you provided `", action, "`")
                 )
              )
  }

}
