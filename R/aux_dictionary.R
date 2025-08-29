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
                           tag     = NULL) {
  measure <- "dictionary"

  wrk_release <- get_from_auxenv(key = "wrk_release")

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

  # ----- function raw sha ------
  raw_sha_fun <- digest::digest(body(
    paste0("aux_", measure))
  )

  setattr(df,
          "raw_sha_fun",
          raw_sha_fun)

    saved <- pip_aux_save(
      x        = df,
      pin_name = measure,
      #metadata = cl_metadata,
      force    = force
    )

    return(invisible(saved))

  } else {
    pipload::load_aux_data(measure = measure)

  }
}
