#' PIP Special National accounts
#'
#' Update special national accounts data
#'
#' @inheritParams pip_pfw
#' @inheritParams pipfun::load_from_gh
#' @param from character: Either "gh", "file" or "api". Default is "gh". "file"
#'   and "gh" are synonymous
#' @export
pip_sna <- function(action          = c("update", "load"),
                    force           = FALSE,
                    maindir         = gls$PIP_DATA_DIR,
                    owner           = getOption("pipfun.ghowner"),
                    branch          = c("DEV", "PROD", "main"),
                    tag             = match.arg(branch)) {

  measure    <- "sna"
  branch <- match.arg(branch)
  action <- match.arg(action)


  if (action == "update") {
    # load nowcast growth rates
    sna <- pipfun::load_from_gh(
      measure = "sna",
      owner  = owner,
      branch = branch
    )
    if (branch == "main") {
      branch <- ""
    }
    msrdir <- fs::path(maindir, "_aux", branch, measure) # measure dir

    saved <- pipfun::pip_sign_save(
      x       = sna,
      measure = measure,
      msrdir  = msrdir,
      force   = force
    )

  } else {
    dt <- load_aux(
      maindir = maindir,
      measure = measure,
      branch  = branch
    )
    return(dt)
  }
} # end of pip_gdp
