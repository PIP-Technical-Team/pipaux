#' PIP Indicators
#'
#' Update or load a dataset with the indicators master sheet.
#'
#' @inheritParams pip_pfw
#' @inheritParams pipfun::load_from_gh
#' @export
pip_indicators <- function(action  = c("update", "load"),
                           force   = FALSE,
                           owner   = getOption("pipfun.ghowner"),
                           maindir = gls$PIP_DATA_DIR,
                           branch  = c("DEV", "PROD", "main"),
                           tag     = match.arg(branch)) {
  measure <- "indicators"
  branch <- match.arg(branch)
  action <- match.arg(action)


  if (action == "update") {
    df <-
      pipfun::load_from_gh(
        measure = measure,
        owner  = owner,
        branch = branch
      )

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
    msrdir <- fs::path(maindir, "_aux", branch, measure) # measure dir
    saved  <- pipfun::pip_sign_save(
      x       = df,
      measure = measure,
      msrdir  = msrdir,
      force   = force
    )
    return(invisible(saved))
  } else  {
    df <- load_aux(
      maindir = maindir,
      measure = measure,
      branch  = branch
    )
    return(df)
  }
}
