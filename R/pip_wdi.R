#' PIP wdi
#'
#' Update or load wdi data.
#'
#' @inheritParams pip_pfw
#' @inheritParams pipfun::load_from_gh
#' @param from character: Either "gh", "file" or "api". Default is "gh". "file"
#'   and "gh" are synonymous
#' @export
pip_wdi <- function(action          = c("update", "load"),
                    force           = FALSE,
                    maindir         = gls$PIP_DATA_DIR,
                    owner           = getOption("pipfun.ghowner"),
                    branch          = c("DEV", "PROD", "main"),
                    tag             = match.arg(branch),
                    from            = c("gh", "file", "api")) {

  measure    <- "wdi"
  branch <- match.arg(branch)
  action <- match.arg(action)


  if (action == "update") {
    pip_wdi_update(maindir = maindir,
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
} # end of pip_wdi
