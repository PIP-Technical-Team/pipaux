#' Fetch GDP data from WEO
#'
#' Create a dataset with GDP data from World Economic Outlook.
#'
#' Note that the most recent version most be downloaded from imf.org and saved
#' as an .xls file in `<maindir>/_aux/weo/`. The filename should be in the
#' following structure `WEO_<YYYY-DD-MM>.xls`. Due to potential file corruption
#' the file must be opened and re-saved before it can be updated with
#' `pip_weo()`. Hopefully in the future IMF will stop using an `.xls` file
#' that's not really xls.
#'
#' @inheritParams pip_prices
#' @export
pip_weo <- function(action  = c("update", "load"),
                    force   = FALSE,
                    owner   = getOption("pipaux.ghowner"),
                    maindir = gls$PIP_DATA_DIR,
                    branch  = c("DEV", "PROD", "main"),
                    tag     = match.arg(branch)) {
  measure <- "weo"
  branch <- match.arg(branch)
  action <- match.arg(action)

  if (action == "update") {

    # ---- Load data from disk ----

    # Read data
    dt <- load_raw_aux(
      measure = measure,
      owner  = owner,
      branch = branch,
      tag    = tag
    )
    dt <- pip_weo_clean(dt,
                        maindir = maindir,
                        branch = branch)

    # Save dataset
    msrdir <- fs::path(maindir, "_aux", branch, measure) # measure dir
    pip_sign_save(
      x       = dt,
      measure = measure,
      msrdir  = msrdir,
      force   = force
    )
  } else if (action == "load") {
    dt <- load_aux(
      maindir = maindir,
      measure = measure
    )
    return(dt)
  } else {
    rlang::abort(c("`action` must be `update` or `load`",
      x = paste0("you provided `", action, "`")
    ))
  }
}
