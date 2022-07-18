#' PIP PCE
#'
#' Load or update PCE data.
#'
#' @inheritParams pip_prices
#' @param  sna_branch character: release tag of pip-sna file needed. Default is "main"
#' @export
pip_pce <- function(action  = c("update", "load"),
                    force   = FALSE,
                    owner   = getOption("pipaux.ghowner"),
                    maindir = gls$PIP_DATA_DIR,
                    branch  = c("DEV", "PROD", "main"),
                    tag     = match.arg(branch),
                    from    = c("gh", "file", "api")) {
  measure <- "pce"
  branch <- match.arg(branch)
  action <- match.arg(action)

  if (action == "update") {
    pip_pce_update(maindir = maindir,
                   force   = force,
                   owner   = owner,
                   branch  = branch,
                   tag     = tag,
                   from    = from)

  } else {
    dt <- load_aux(
      maindir = maindir,
      measure = measure,
      branch  = branch
    )
    return(dt)
  }
}
