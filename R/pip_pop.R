#' PIP POP
#'
#' Load or update population data.
#'
#' @inheritParams pip_cpi
#' @inheritParams pipfun::load_from_gh
#' @param src character: Source for population data. Defaults to `getOption("pipaux.popsrc")`.
#' @export
pip_pop <- function(action = c("update", "load"),
                    force   = FALSE,
                    from    = c("gh", "file", "api"),
                    maindir = gls$PIP_DATA_DIR,
                    owner   = getOption("pipfun.ghowner"),
                    branch  = c("DEV", "PROD", "main"),
                    tag     = match.arg(branch)) {
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
      tag     = tag )

  } else {

    df <- load_aux(maindir = maindir,
                   measure = measure,
                   branch = branch)

    return(df)
  }
}
