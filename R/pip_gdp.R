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
                    owner           = "PIP-Technical-Team",
                    branch          = c("DEV", "PROD", "main"),
                    tag             = match.arg(branch),
                    from            = "file") {

  measure    <- "gdp"
  branch <- match.arg(branch)


  if (action == "update") {
    pip_gdp_update(maindir = maindir,
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
} # end of pip_gdp
