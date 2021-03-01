#' PIP GDM
#'
#' Load or update grouped data means dataset from PovcalNet Masterfile.
#'
#' @inheritParams pip_prices
#' @param pcndir character: PovcalNet Masterfile directory.
#' @export
pip_gdm <- function(action = "update",
                    force  = FALSE,
                    pcndir = getOption("pipaux.pcnsrc"),
                    maindir = getOption("pipaux.maindir")) {

  measure <- "gdm"

  if (action == "update") {

    pip_gdm_update(force = force, maindir = maindir, pcndir = pcndir)

  } else if (action == "load") {

    df <- load_aux(maindir  = maindir,
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
