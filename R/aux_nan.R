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
                    owner           = getOption("pipfun.ghowner"),
                    tag             = NULL) {

  measure    <- "nan"
  action <- match.arg(action)

  wrk_release <- get_from_auxenv(key = "wrk_release")

  release        <- wrk_release$release
  identity       <- wrk_release$identity
  branch         <- paste0(release, "_", identity)

  if (is.null(tag)) {
    tag <- paste0(release, "_", identity)
  }


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


    # ----- function raw sha ----------------------
    raw_sha_fun <- digest::digest(body(
      paste0("aux_", measure))
    )

    key_cols <- c("country_code", "year")
    setattr(nan, "aux_key", key_cols)
    setattr(nan, "raw_sha_fun", raw_sha_fun)

    saved <- pip_aux_save(
      x        = nan,
      id       = measure,
      pk       = key_cols,
      force    = force
    )

  } else {
    dt <- pipload::load_aux_data(measure = measure)

    return(dt)
  }
}
