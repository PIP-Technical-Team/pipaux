#' PIP Regions
#'
#' Update or load a dataset with regions.
#'
#' @inheritParams aux_cpi
#' @inheritParams pipfun::load_from_gh
#' @export
aux_regions <- function(action  = c("update", "load"),
                        owner   = getOption("pipfun.ghowner"),
                        tag     = NULL,
                        verbose = FALSE
                        ) {


  measure <- "regions"
  action  <- match.arg(action)

  wrk_release <- get_from_auxenv(key = "wrk_release")

  release        <- wrk_release$release
  identity       <- wrk_release$identity
  branch         <- paste0(release, "_", identity)

  if (is.null(tag)) {
    tag <- paste0(release, "_", identity)
  }

  if (action == "update") {

    ##  ............................................................................
    ##  Load country_list table                                                 ####

    cl <- pipload::load_aux_data(measure = "country_list", verbose = verbose)

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
    #dt <- dt[grouping_type != "pcn_region" & region_code != ""]
    dt <- dt[grouping_type != "region" & region_code != ""]
    setorder(dt, grouping_type, region_code)



##  ............................................................................
##  Save data                                                               ####

    if (branch == "main") {
    branch <- ""
  }


  setattr(dt, "aux_name", "regions")
  key_cols <- c("region_code")
  setattr(dt, "aux_key", key_cols)

    saved <- pip_aux_save(
      x        = dt,
      id       = measure,
      pk       = key_cols,
      code     = aux_regions,
      code_label = "aux_regions",
      verbose  = verbose
    )

    return(invisible(saved))


  } else {

    df <- pipload::load_aux_data(measure = measure, verbose = verbose)

    return(df)
  }

}




