#' Update PFW
#'
#' @inheritParams pip_pfw
#' @inheritParams pipfun::load_from_gh
#' @keywords internal
pip_pfw_update <- function(maindir = gls$PIP_DATA_DIR,
                           force = FALSE,
                           owner   = getOption("pipfun.ghowner"),
                           branch  = c("DEV", "PROD", "main"),
                           tag     = match.arg(branch),
                           detail  = getOption("pipaux.detail.raw")) {

  measure <- "pfw"
  branch <- match.arg(branch)

  # Read data
  pfw <- pipfun::load_from_gh(measure = measure,
                      owner = owner,
                      branch = branch,
                      ext = "dta")
  # validate pfw raw data
  pfw_validate_raw(pfw = pfw, detail = detail)

  # Clean data
  pfw <- pip_pfw_clean(pfw,
                       maindir = maindir,
                       branch = branch)

  # validate pfw raw data
  pfw_validate_output(pfw = pfw, detail = detail)

  # Save dataset
  if (branch == "main") {
    branch <- ""
  }
  msrdir <- fs::path(maindir, "_aux", branch, measure) # measure dir

  setattr(pfw, "aux_name", "pfw")

  saved <- pipfun::pip_sign_save(
    x       = pfw,
    measure = measure,
    msrdir  = msrdir,
    force   = force
  )
  return(invisible(saved))
}
