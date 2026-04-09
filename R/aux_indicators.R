#' PIP Indicators
#'
#' Update or load a dataset with the indicators master sheet.
#'
#' @inheritParams aux_pfw
#' @inheritParams pipfun::load_from_gh
#' @export
aux_indicators <- function(action  = c("update", "load"),
                           owner   = getOption("pipfun.ghowner"),
                           verbose = FALSE,
                           tag     = NULL) {
  measure <- "indicators"
  action <- match.arg(action)

  wrk_release <- get_from_auxenv(key = "wrk_release")

  release        <- wrk_release$release
  identity       <- wrk_release$identity
  branch         <- paste0(release, "_", identity)

  if (is.null(tag)) {
    tag <- paste0(release, "_", identity)
  }

  if (action == "update") {
    df <-
      pipfun::load_from_gh(
        measure = measure,
        owner  = owner,
        branch = branch,
        ext    = "csv"
      )

    gh <- attributes(df)$gh

    # Convert empty strings to NA in all character variables
    chr_df <-
      sapply(df, is.character) |>
      which() |>
      names()

    df[, (chr_df) :=
         lapply(.SD, \(x) {
           x <- fifelse(x == "", NA_character_, x)
         }),
       .SDcols = chr_df]



   # Save dataset
  if (branch == "main") {
    branch <- ""
  }
    
  key_cols <- c("page", "indicator_name")
    

  saved <- pip_aux_save(
      x        = df,
      id       = measure,
      pk       = key_cols,
      metadata = list(gh = gh),
      code     = aux_indicators,
      code_label = "aux_indicators"
    )

    return(invisible(saved))

  } else  {

    df <- pipload::load_aux_data(measure = measure, verbose = verbose)

    return(df)
  }
}
