#' Update and load GDP data in PIP Auxiliary data structure
#'
#' @param action
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
pip_gdp <- function(action          = "update",
                    maddison_action = "load",
                    force           = FALSE) {


  measure   <- "gdp"
  r         <- pip_aux_values()
  # update Maddison Project Data
  if (maddison_action == "update") {
    pip_maddison(force = force)
  }

  if (action == "update") {

    pip_gdp_update(force = force)

  } else if (action == "load") {
    msrdir    <- paste0(getOption("pipaux.maindir"), "_aux/", measure, "/")  # measure dir
    pip_aux_load(msrdir = msrdir,
                 measure = measure)

  }  # End of update


} # end of pip_gdp







# DT = data.table(year=2010:2014, v1=runif(5), v2=1:5, v3=letters[1:5])
# DT[, shift(.SD, 1:2, NA, "lead", TRUE), .SDcols=2:4]
#
# DT[,
#    lag_1 := shift(v2, n = -1, type = "lag")
#    ][
#      ,
#      lead_1 := shift(v2, n = -1, type = "lead")
#    ][,
#      lag1 := shift(v2, n = 1, type = "lag") # default
#    ][
#      ,
#      lead1 := shift(v2, n = 1, type = "lead")
#    ]











