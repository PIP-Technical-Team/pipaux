#' Poverty lines
#'
#' Update or load a dataset with poverty lines.
#'
#' @inheritParams pip_prices
#' @inheritParams load_raw_aux
#' @export
pip_pl <- function(action = c("update", "load"),
                   force = FALSE,
                   owner   = getOption("pipaux.ghowner"),
                   maindir = gls$PIP_DATA_DIR,
                   branch  = c("DEV", "PROD", "main"),
                   tag     = match.arg(branch)) {

  measure <- "pl"
  branch <- match.arg(branch)
  action <- match.arg(action)


  if (action == "update") {
    # Read yaml file

    dl <- load_raw_aux(
      measure = measure,
      owner  = owner,
      branch = branch,
      tag    = tag,
      ext    = "yaml"
    )

    dt <- purrr::map_df(dl,pip_pl_clean)

    # Save
    msrdir <- fs::path(maindir, "_aux", branch, measure) # measure dir
    saved <- pip_sign_save(
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
