#' Maddison Project Data
#'
#' @inheritParams pip_prices
#' @return
#' @export
#' @import data.table
#'
#' @examples
pip_maddison <- function(action = "update",
                         force  = FALSE,
                         maindir = getOption("pipaux.maindir")) {

  measure <- "maddison"
  msrdir  <- paste0(maindir, "_aux/", measure, "/") # measure dir

  if (action == "update") {

    mpd <- haven::read_dta("https://www.rug.nl/ggdc/historicaldevelopment/maddison/data/mpd2018.dta")

    setDT(mpd)
    setnames(mpd,
             old = c("countrycode", "country", "rgdpnapc"),
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
