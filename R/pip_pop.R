#' Load or Update population data
#'
#' @param action character: Whether to update or load. Default `update`
#' @param force logical: if TRUE Pop data will be updated. Required `action = "update"`
#' @param src character: Source fo Population data. Default is `wdi`. Other
#' option is `emi`
#' @param maindir character: Directory path of main folder. Default `getOption("pipaux.maindir")`
#' @param ... character: Additional parameters
#'
#' @return
#' @export
#'
#' @examples
pip_pop <- function(action  = "update",
                    force   = FALSE,
                    src     = getOption("pipaux.popsrc"),
                    maindir = getOption("pipaux.maindir"),
                    ...){

  measure <- "pop"
  msrdir  <- paste0(maindir, "_aux/", measure, "/")  # measure dir
  src     <- tolower(src)

  if (action == "update") {

    pip_pop_update(force  = force,
                   src    = src,
                   msrdir = msrdir)

  } else if (action == "load") {

    df <- load_aux(msrdir  = msrdir,
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
