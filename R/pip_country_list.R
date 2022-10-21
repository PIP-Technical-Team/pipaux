#' List of countries
#'
#' Load or update dataset with WDI countries. See details.
#'
#' This function creates a combined dataset of countries in WDI and their
#' respective regional classification by querying `wbstats::wb_countries()`, as
#' well as reading from the PovcalNet Masterfile to fetch PCN region codes.
#'
#' The dependency on the PCN Masterfile should be changed in the future.
#'
#' @inheritParams pip_pfw
#' @inheritParams pipfun::load_from_gh
#' @export
#' @return logical if `action = "update"` or data.table if `action = "load"`
pip_country_list <- function(action = c("update", "load"),
                             maindir = gls$PIP_DATA_DIR,
                             force   = FALSE,
                             owner   = getOption("pipfun.ghowner"),
                             branch  = c("DEV", "PROD", "main"),
                             tag     = match.arg(branch)
                             ) {
  measure <- "country_list"
  branch  <- match.arg(branch)
  action  <- match.arg(action)

  if (action == "update") {

    ## Special national accounts --------
    cl <- pipfun::load_from_gh(
      measure = measure,
      owner  = owner,
      branch = branch,
      tag    = tag
    )

    # Save
    msrdir <- fs::path(maindir, "_aux", branch, measure) # measure dir
    saved <- pipfun::pip_sign_save(
      x       = cl,
      measure = measure,
      msrdir  = msrdir,
      force   = force
    )

    return(invisible(saved))

  } else {

    df <- load_aux(maindir = maindir,
                   measure = measure,
                   branch  = branch)
    return(df)
  }
}
