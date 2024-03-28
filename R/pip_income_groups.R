#' PIP series of income group
#'
#' Update or load a dataset with historical income groups. The raw files are not
#' available in the PIP-Technical-Team group but in the Povcalnet-team group.
#'
#' @param detail has an option TRUE/FALSE, default value is FALSE
#' @inheritParams pip_cpi
#' @inheritParams pipfun::load_from_gh
#' @export
pip_income_groups <- function(action       = c("update", "load"),
                              force        = FALSE,
                              owner        = getOption("pipfun.ghowner"),
                              maindir      = gls$PIP_DATA_DIR,
                              branch       = c("DEV", "PROD", "main"),
                              class_branch = "master",
                              detail       = getOption("pipaux.detail.raw")
) {

  measure <- "income_groups"
  action <- match.arg(action)
  branch <- match.arg(branch)

  if (action == "update") {

    ## Special national accounts --------
    ig <- pipfun::load_from_gh(
      measure  = measure,
      owner    = "GPID-WB",
      repo     = "Class",
      branch   = class_branch,
      filename = "OutputData/CLASS",
      ext      = "dta"
    ) |>
      get_vars(c('code',
                 'year_data',
                 'incgroup_historical',
                 'fcv_historical',
                 'region_SSA')) |>
      # create variables for future development
      ftransform(year         = year_data,
                 income_group = incgroup_historical)

    ig[,
       income_group_code := fcase(income_group == "High income", "HIC",
                                  income_group == "Upper middle income", "UMIC",
                                  income_group == "Lower middle income", "LMIC",
                                  income_group == "Low income", "LIC",
                                  default = "")]
    setnames(ig,
             c("code", "region_SSA"),
             c("country_code", "ssa_subregion_code"))


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## save --------

    # validate income group output data
    incgroup_validate_output(incgroup = ig, detail = detail)

    if (branch == "main") {
      branch <- ""
    }
    msrdir <- fs::path(maindir, "_aux", branch, measure) # measure dir


    saved <- pipfun::pip_sign_save(
      x = ig,
      measure = measure,
      msrdir = msrdir,
      force = force
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
