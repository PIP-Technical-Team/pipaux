#' PIP WDI
#'
#' Update WDI data.
#'
#' @inheritParams pip_cpi
#' @inheritParams pipfun::load_from_gh
#' @param src character: Source for population data. Defaults to `getOption("pipaux.popsrc")`.
#' @export
pip_wdi <- function(force   = FALSE,
                    from    = c("gh", "file", "api"),
                    maindir = gls$PIP_DATA_DIR,
                    owner   = getOption("pipfun.ghowner"),
                    branch  = c("DEV", "PROD", "main", "test"),
                    tag     = match.arg(branch)) {
  measure <- "pop"
  from    <- tolower(from)

  pip_pop_update(
    force   = force,
    from    = from,
    maindir = maindir,
    owner   = owner,
    branch  = branch,
    tag     = tag )
}
