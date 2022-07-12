#' PIP GDP
#'
#' Update or load GDP data.
#'
#' @inheritParams pip_prices
#' @inheritParams load_raw_aux
#' @export
pip_gdp <- function(action          = "update",
                    force           = FALSE,
                    maindir         = gls$PIP_DATA_DIR,
                    owner           = "PIP-Technical-Team",
                    branch          = c("DEV", "PROD", "main"),
                    tag             = match.arg(branch),
                    from            = "file") {

  measure    <- "gdp"
  branch <- match.arg(branch)


  if (action == "update") {
    pip_gdp_update(force   = force,
                   maindir = maindir,
                   branch  = branch)

  } else if (action == "load") {
    load_aux(
      maindir = maindir,
      measure = measure
    )
  } # End of update
} # end of pip_gdp
