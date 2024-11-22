#' PIP PCE
#'
#' Load or update PCE data.
#'
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @inheritParams aux_gdp
#' @inheritParams aux_pfw
#' @inheritParams pipfun::load_from_gh
#' @export
aux_pce <- function(action  = c("update", "load"),
                    force   = FALSE,
                    owner   = getOption("pipfun.ghowner"),
                    maindir = gls$PIP_DATA_DIR,
                    branch  = c("DEV", "PROD", "main"),
                    tag     = match.arg(branch),
                    from    = c("gh", "file", "api"),
                    detail  = getOption("pipaux.detail.raw")) {
  measure <- "pce"
  branch <- match.arg(branch)
  action <- match.arg(action)

  if (action == "update") {
    aux_pce_update(maindir = maindir,
                   force   = force,
                   owner   = owner,
                   branch  = branch,
                   tag     = tag,
                   from    = from,
                   detail  = detail)

  } else {
    dt <- load_aux(
      maindir = maindir,
      measure = measure,
      branch  = branch
    )
    return(dt)
  }
}
