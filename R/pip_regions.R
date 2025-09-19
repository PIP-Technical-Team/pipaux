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

    cl <- load_aux(maindir = maindir,
                   measure = "country_list",
                   branch  = branch)

    setnames(cl, "country_code", "id") # to make it work w/o problems


    vars_code <- grep("_code", names(cl), value = TRUE)
    vars      <- gsub("_code", "", vars_code)

    dt <- lapply(seq_along(vars), \(.) {
      vc <- vars_code[.]
      vn <- vars[.]
      CD <- cl[, mget(c(vc, vn))] |>
        unique()
      setnames(CD, new = c("region_code", "region"))
      CD[, grouping_type := vn]
    }) |>
      rbindlist(fill = TRUE) |>
      na_omit()
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




