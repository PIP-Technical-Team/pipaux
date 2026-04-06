#' PIP Dictionary
#'
#' Update or load a dataset with the indicators master sheet.
#'
#' @inheritParams aux_pfw
#' @inheritParams pipfun::load_from_gh
#' @export
aux_dictionary <- function(action  = c("update", "load"),
                           owner   = getOption("pipfun.ghowner"),
                           tag     = NULL,
                           verbose = FALSE) {
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
    gh <- attributes(df)$gh
    # Save dataset
    if (branch == "main") {
    branch <- ""
  }
    # Define key columns for dictionary data
    key_cols <- names(df)
    setattr(df, "aux_key", key_cols)
    saved <- pip_aux_save(
      x        = df,
      id       = measure,
      pk       = key_cols,
      metadata = list(gh = gh),
      code     = aux_dictionary,
      code_label = "aux_dictionary",
      verbose  = verbose
    )

    return(invisible(saved))

  } else {
    pipload::load_aux_data(measure = measure, verbose = verbose)

  }
}
