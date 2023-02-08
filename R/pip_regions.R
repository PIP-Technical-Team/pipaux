#' PIP Regions
#'
#' Update or load a dataset with regions.
#'
#' @inheritParams pip_cpi
#' @inheritParams pipfun::load_from_gh
#' @export
pip_regions <- function(action = c("update", "load"),
                        force = FALSE,
                        maindir = gls$PIP_DATA_DIR,
                        owner   = getOption("pipfun.ghowner"),
                        branch  = c("DEV", "PROD", "main"),
                        tag     = match.arg(branch)
                        ) {


  measure <- "regions"
  action  <- match.arg(action)
  branch  <- match.arg(branch)

  if (action == "update") {

    ##  ............................................................................
    ##  Load country_list table                                                 ####

    cl <- pipfun::load_from_gh(
      measure = "country_list",
      owner   = owner,
      branch  = branch,
      tag     = tag
    )

    setnames(cl, "country_code", "id") # to make it work w/o problems

    ##  ............................................................................
    ##  get code variables                                                      ####


    ml <- melt(cl,
               id.vars         = c("id"),
               measure.vars    = patterns("code$"),
               variable.factor = FALSE,
               value.factor    = FALSE,
               value.name      = "region_code",
               variable.name   = "grouping_type")

    ml[,
       grouping_type := gsub("_code", "", grouping_type)]

    ##  ............................................................................
    ##  Get label variables                                                     ####

    grs <- ml[, unique(grouping_type) ] |>
      {\(.) c("id",.) }()

    ml2 <- melt(cl[, ..grs],
                id.vars         = c("id"),
                variable.factor = FALSE,
                value.factor    = FALSE,
                value.name      = "region",
                variable.name   = "grouping_type")
    ##  ............................................................................
    ##  Merge ml and ml2                                                        ####

    dt <- joyn::merge(ml, ml2,
                      by         = c("id", "grouping_type"),
                      match_type = "1:1",
                      verbose    = FALSE)

    ### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
    ### Get unique database                                                     ####

    byv <- c("region", "region_code", "grouping_type")
    dt <- unique(dt[, ..byv], by = byv)
    dt <- dt[grouping_type != "pcn_region" & region_code != ""]
    setorder(dt, grouping_type, region_code)



##  ............................................................................
##  Save data                                                               ####

    if (branch == "main") {
    branch <- ""
  }
  msrdir <- fs::path(maindir, "_aux", branch, measure) # measure dir
    saved <- pipfun::pip_sign_save(
      x       = dt,
      measure = measure,
      msrdir  = msrdir,
      force   = force
    )
    return(invisible(saved))


  } else {
    df <- load_aux(
      maindir = maindir,
      measure = measure,
      branch  = branch
    )
    return(df)
  }

} # end of function




