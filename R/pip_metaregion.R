#' Metadata for PIP regions
#'
#' Update or load a dataset with regions.
#'
#' @inheritParams pip_cpi
#' @inheritParams pipfun::load_from_gh
#' @export
pip_metaregion <- function(action = c("update", "load"),
                        force = FALSE,
                        maindir = gls$PIP_DATA_DIR,
                        owner   = getOption("pipfun.ghowner"),
                        branch  = c("DEV", "PROD", "main"),
                        tag     = match.arg(branch)
) {
  measure <- "metaregion"
  action  <- match.arg(action)
  branch  <- match.arg(branch)

  if (action == "update") {
    mr <- pipfun::load_from_gh(measure = measure,
                              owner    = owner,
                              branch   = branch)


    ##  ............................................................................
    ##  Save data                                                               ####

    if (branch == "main") {
      branch <- ""
    }
    msrdir <- fs::path(maindir, "_aux", branch, measure) # measure dir
    saved <- pipfun::pip_sign_save(
      x       = mr,
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

} # end of function




