#' Load or update PCE Auxiliary data.
#'
#' @param action character: either `update` or `load`
#' @param force logical: if TRUE force update of PCE.
#'
#' @return
#' @export
#'
#' @examples
pip_pce <- function(action = "update",
                    force  = FALSE) {

  measure <- "pce"
  r       <- pip_aux_values()
  msrdir  <- paste0(getOption("pipaux.maindir"), "_aux/", measure, "/")  # measure dir

  if (action == "update") {

    pip_pce_update(force = force)

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
