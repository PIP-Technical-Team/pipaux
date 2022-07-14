#' Update PFW
#'
#' @inheritParams pip_prices
#' @inheritParams load_raw_aux
#' @keywords internal
pip_pfw_update <- function(maindir = gls$PIP_DATA_DIR,
                           force = FALSE,
                           owner   = getOption("pipaux.ghowner"),
                           branch  = c("DEV", "PROD", "main"),
                           tag     = match.arg(branch)) {

  measure <- "pfw"
  branch <- match.arg(branch)

  # Read data
  pfw <- load_raw_aux(measure = measure,
                      owner = owner,
                      branch = branch,
                      ext = "dta")
  # Clean data
  pfw <- pip_pfw_clean(pfw,
                       maindir = maindir,
                       branch = branch)

  # Save dataset
  msrdir <- fs::path(maindir, "_aux", branch, measure) # measure dir
  saved <- pip_sign_save(
    x       = pfw,
    measure = measure,
    msrdir  = msrdir,
    force   = force
  )
  return(invisible(saved))
}
