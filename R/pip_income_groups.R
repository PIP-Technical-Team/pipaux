#' PIP income groups
#'
#' Update or load a dataset with historical income groups
#'
#' @inheritParams pip_prices
#' @export
pip_income_groups <- function(action = "update",
                              force = FALSE,
                              maindir = gls$PIP_DATA_DIR) {
  measure <- "income_groups"
  msrdir <- paste0(maindir, "_aux/", measure, "/")

  if (action == "update") {
    df <- haven::read_dta(paste0(msrdir, "CLASS.dta"))
    df <- df[c('code', 'year_data', 'incgroup_historical',
               'fcv_historical', 'region_SSA')]
    names(df) <- c('country_code', 'year_data',
                   'incgroup_historical',
                   'fcv_historical',
                   'ssa_subregion_code')
    pip_sign_save(
      x = df,
      measure = measure,
      msrdir = msrdir,
      force = force
    )
  } else if (action == "load") {
    df <- load_aux(
      maindir = maindir,
      measure = measure
    )
    return(df)
  } else {
    msg <- paste("action `", action, "` is not a valid action.")
    rlang::abort(c(
      msg,
      i = "make sure you select `update` or `load`"
    ),
    class = "pipaux_error"
    )
  }
}
