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
                           maindir = getOption("pipaux.working_dir"),
                           tag     = NULL) {
  measure <- "dictionary"

  pipfun::get_wrk_release(verbose = FALSE)

  release        <- wrk_release$release
  identity       <- wrk_release$identity
  branch         <- paste0(release, "_", identity)

  if (is.null(tag)) {
    tag <- paste0(release, "_", identity)
  }

  action <- match.arg(action)

  if (action == "update") {

    df <- pipfun::load_from_gh(measure = measure,
                       owner           = owner,
                       branch          = branch,
                       tag             = tag,
                       ext             = "csv")
    # Save dataset
    if (branch == "main") {
    branch <- ""
  }
  msrdir <- fs::path(maindir, "aux_data", branch, measure) # measure dir

  # ----- function raw sha ------
  raw_sha_fun <- digest::digest(body(
    paste0("aux_", measure))
  )

  setattr(df,
          "raw_sha_fun",
          raw_sha_fun)

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
