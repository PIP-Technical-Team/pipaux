#' PIP Dictionary
#'
#' Update or load a dataset with the indicators master sheet.
#'
#' @inheritParams aux_pfw
#' @inheritParams pipfun::load_from_gh
#' @export
aux_dictionary <- function(action  = c("update", "load"),
                           force   = FALSE,
                           owner   = getOption("pipfun.ghowner"),
                           maindir = gls$PIP_DATA_DIR,
                           tag     = match.arg(branch)) {
  measure <- "dictionary"

  pipfun::get_wrk_release(verbose = FALSE)

  release        <- wrk_release$release
  identity       <- wrk_release$identity
  branch         <- paste0(release, "_", identity)

  action <- match.arg(action)

  if (action == "update") {

    df <- pipfun::load_from_gh(measure = measure,
                       owner = owner,
                       branch = branch,
                       tag = tag)
    # Save dataset
    if (branch == "main") {
    branch <- ""
  }
  msrdir <- fs::path(maindir, "_aux", branch, measure) # measure dir
    saved <- pipfun::pip_sign_save(
      x       = df,
      measure = measure,
      msrdir  = msrdir,
      force   = force
    )

    return(invisible(saved))

  } else {
    load_aux(
      maindir = maindir,
      measure = measure,
      branch  = branch
    )
  }
}
