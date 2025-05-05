#' PIP nowcast data
#'
#' Update nowcast data
#'
#' @inheritParams aux_pfw
#' @inheritParams pipfun::load_from_gh
#' @param from character: Either "gh", "file" or "api". Default is "gh". "file"
#'   and "gh" are synonymous
#' @export
aux_nan <- function(action          = c("update", "load"),
                    force           = FALSE,
                    maindir         = gls$PIP_DATA_DIR,
                    owner           = getOption("pipfun.ghowner"),
                    tag             = match.arg(branch)) {

  measure    <- "nan"
  action <- match.arg(action)

  pipfun::get_wrk_release(verbose = FALSE)

  release        <- wrk_release$release
  identity       <- wrk_release$identity
  branch         <- paste0(release, "_", identity)


  if (action == "update") {
    # load nowcast growth rates
    nan <- pipfun::load_from_gh(
      measure = "nan",
      owner  = owner,
      branch = branch,
      filename = "nan.csv"
    )
    if (branch == "main") {
      branch <- ""
    }
    msrdir <- fs::path(maindir, "aux_data", branch, measure) # measure dir

    # ----- function raw sha ----------------------

    raw_sha_fun <- digest::digest(body(
      paste0("aux_", measure))
    )


    setattr(nan,
            "raw_sha_fun",
            raw_sha_fun)

    saved <- pipfun::pip_sign_save(
      x       = nan,
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
