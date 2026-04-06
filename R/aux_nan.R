#' PIP nowcast data
#'
#' Update nowcast data
#'
#' @inheritParams aux_pfw
#' @inheritParams pipfun::load_from_gh
#' @export
aux_nan <- function(action          = c("update", "load"),
                    owner           = getOption("pipfun.ghowner"),
                    tag             = NULL,
                    verbose         = FALSE) {

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
      owner  = "PIP-Technical-Team",
      branch = branch,
      filename = "nan.csv"
    )

    gh <- attributes(nan)$gh

    if (branch == "main") {
      branch <- ""
    }

    key_cols <- c("country_code", "year", "gdp_data_level")
    setattr(nan, "aux_key", key_cols)

    saved <- pip_aux_save(
      x        = nan,
      id       = measure,
      pk       = key_cols,
      metadata = list(gh = gh),
      code     = aux_nan,
      code_label = "aux_nan",
      verbose  = verbose
    )

  } else {
    dt <- pipload::load_aux_data(measure = measure, verbose = verbose)

    return(dt)
  }
}
