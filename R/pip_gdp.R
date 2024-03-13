#' PIP GDP
#'
#' Update or load GDP data.
#'
#' @inheritParams pip_pfw
#' @inheritParams pipfun::load_from_gh
#' @param from character: Either "gh", "file" or "api". Default is "gh". "file"
#'   and "gh" are synonymous
#' @export
pip_gdp <- function(action          = c("update", "load"),
                    force           = FALSE,
                    maindir         = gls$PIP_DATA_DIR,
                    owner           = getOption("pipfun.ghowner"),
                    branch          = c("DEV", "PROD", "main"),
                    tag             = match.arg(branch),
                    from            = "file",
                    detail          = getOption("pipaux.detail.raw")) {

  measure    <- "gdp"
  branch <- match.arg(branch)
  action <- match.arg(action)


  if (action == "update") {
    pip_gdp_update(maindir = maindir,
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
} # end of pip_gdp
