#' PIP GDP
#'
#' Update or load GDP data.
#'
#' @inheritParams pip_prices
#' @inheritParams load_raw_aux
#' @param maddison_action character: Either "load" or "update". Default is "update".
#' @param weo_action character: Either "load" or "update". Default is "update".
#' @export
pip_gdp <- function(action          = "update",
                    maddison_action = "load",
                    weo_action      = "load",
                    force           = FALSE,
                    maindir         = gls$PIP_DATA_DIR,
                    owner           = "PIP-Technical-Team",
                    branch          = c("DEV", "PROD", "main"),
                    tag             = match.arg(branch),
                    from            = "file") {

  measure    <- "gdp"
  sna_branch <- match.arg(sna_branch)

  # Update Maddison Project Data
  if (maddison_action == "update") {
    pip_maddison(force = force, maindir = maindir)
  }

  # Update WEO data
  if (weo_action == "update") {
    pip_gdp_weo(force = force, maindir = maindir)
  }

  if (action == "update") {
    pip_gdp_update(force      = force,
                   maindir    = maindir,
                   sna_branch = sna_branch)

  } else if (action == "load") {
    load_aux(
      maindir = maindir,
      measure = measure
    )
  } # End of update
} # end of pip_gdp
