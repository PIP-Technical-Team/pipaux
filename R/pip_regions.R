#' PIP Regions
#'
#' Update or load a dataset with regions.
#'
#' @inheritParams pip_prices
#' @export
pip_regions <- function(action = "update",
                        force = FALSE,
                        maindir = getOption("pipaux.maindir")) {


  measure <- "regions"
  msrdir  <- paste0(maindir, "_aux/", measure, "/")

  if (action == "update") {

    df <- suppressMessages(
      readr::read_csv(paste0(maindir, "_aux/regions/regions.csv"))
    )
    pip_sign_save(x       = df,
                  measure = measure,
                  msrdir  = msrdir,
                  force   = force)

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
