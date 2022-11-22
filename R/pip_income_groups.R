#' PIP income groups
#'
#' Update or load a dataset with historical income groups. The raw files are not
#' available in the PIP-Technical-Team group but in the Povcalnet-team group.
#'
#' @inheritParams pip_pfw
#' @inheritParams pipfun::load_from_gh
#' @export
pip_income_groups <- function(action  = c("update", "load"),
                              force   = FALSE,
                              owner   = "PovcalNet-Team",
                              repo    = "Class",
                              maindir = gls$PIP_DATA_DIR,
                              branch  = c("DEV", "PROD", "main"),
                              tag     = match.arg(branch)) {
  measure <- "income_groups"
  branch <- match.arg(branch)
  action <- match.arg(action)


  if (action == "update") {

    df <- pipfun::load_from_gh(measure = measure,
                       owner   = owner,
                       repo    = repo,
                       branch  = branch,
                       tag     = tag,
                       filename = "OutputData/CLASS",
                       ext = "dta")

    df <- df[,
             c('code',
               'year_data',
               'incgroup_historical',
               'fcv_historical',
               'region_SSA')]
    setnames(df, new = c('country_code', 'year_data',
                         'incgroup_historical',
                         'fcv_historical',
                         'ssa_subregion_code'))


    # save data
    msrdir <- fs::path(maindir, "_aux", branch, measure) # measure dir
    saved <- pipfun::pip_sign_save(
      x       = df,
      measure = measure,
      msrdir  = msrdir,
      force   = force
    )
    return(invisible(saved))

  } else  {

    load_aux(
      maindir = maindir,
      measure = measure,
      branch  = branch
    )

  }
}
