#' Poverty lines
#'
#' Update or load a dataset with poverty lines.
#'
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @inheritParams pip_pfw
#' @inheritParams pipfun::load_from_gh
#' @export
pip_pl <- function(action = c("update", "load"),
                   force = FALSE,
                   owner   = getOption("pipfun.ghowner"),
                   maindir = gls$PIP_DATA_DIR,
                   branch  = c("DEV", "PROD", "main"),
                   tag     = match.arg(branch),
                   detail  = getOption("pipaux.detail.raw")
                   ) {

  measure <- "pl"
  branch <- match.arg(branch)
  action <- match.arg(action)


  if (action == "update") {
    # Read yaml file

    dl <- pipfun::load_from_gh(
      measure = measure,
      owner  = owner,
      branch = branch,
      tag    = tag,
      ext    = "yaml"
    )

    dt <- purrr::map_df(dl,pip_pl_clean)

    # Save

  # validate pl clean data
    pl_validate_output(pl = dt, detail = detail)

    if (branch == "main") {
      branch <- ""
    }
  msrdir <- fs::path(maindir, "_aux", branch, measure) # measure dir
    saved <- pipfun::pip_sign_save(
      x       = dt,
      measure = measure,
      msrdir  = msrdir,
      force   = force
    )

    return(invisible(saved))

  } else {
    df <- load_aux(
      maindir = maindir,
      measure = measure,
      branch  = branch
    )

    return(df)
  }
}
