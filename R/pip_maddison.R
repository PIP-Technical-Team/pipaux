#' Maddison Project Data
#'
#' @param action character: either `load` or `update`
#' @param force logical: if TRUE force update database
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
pip_maddison <- function(action = "update",
                         force  = FALSE) {

  measure <- "maddison"
  r       <- pip_aux_values()
  msrdir  <- paste0(r$maindir, "_aux/", measure, "/") # measure dir

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

    df <- pip_aux_load(msrdir  = msrdir,
                       measure = measure)
    return(df)

  } else {
    rlang::abort(c("`action` must be `update` or `load`",
                 x = paste0("you provided `", action, "`")
                 )
              )
  }

}
