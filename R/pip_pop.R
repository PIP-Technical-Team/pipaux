#' PIP POP
#'
#' Load or update population data.
#'
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @inheritParams pip_cpi
#' @inheritParams pipfun::load_from_gh
#' @param from character: Source for population data.
#' @export
pip_pop <- function(action = c("update", "load"),
                    force   = FALSE,
                    from    = c("gh", "file", "api"),
                    maindir = gls$PIP_DATA_DIR,
                    owner   = getOption("pipfun.ghowner"),
                    branch  = c("DEV", "PROD", "main"),
                    tag     = match.arg(branch),
                    detail  = getOption("pipaux.detail.raw")) {
  measure <- "pop"
  from    <- tolower(from)
  action <- match.arg(action)

  if (action == "update") {
    pip_pop_update(
      force   = force,
      from    = from,
      maindir = maindir,
      owner   = owner,
      branch  = branch,
      tag     = tag,
      detail  = detail)

  } else {

    df <- load_aux(maindir = maindir,
                   measure = measure,
                   branch = branch)

    return(df)
  }
}
