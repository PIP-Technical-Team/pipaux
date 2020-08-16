#' Load or Update population data
#'
#' @param action character: Whether to update or load. Default `update`
#' @param force logical: if TRUE Pop data will be updated. Required `action = "update"`
#' @param src character: Source fo Population data. Default is `wdi`. Other
#' option is `emi`
#'
#' @return
#' @export
#'
#' @examples
pip_pop <- function(action = "update",
                    force  = FALSE,
                    src    = "wdi"){

  measure <- "pop"
  r       <- pip_aux_values()
  msrdir  <- paste0(r$maindir, "_aux/", measure, "/")  # measure dir
  src     <- tolower(src)

  if (action == "update") {

    pip_pop_update(force  = force,
                   src    = src,
                   msrdir = msrdir)

  } else if (action == "load") {

    df <- pip_aux_load(msrdir  = msrdir,
                       measure = measure)
    return(df)


  } else {
    msg <- paste("action `", action,"` is not a valid action.")
    rlang::abort(c(
      msg,
      i = "make sure you select `update` or `load`"
    ),
    class = "pipaux_error"
    )
  }

}
