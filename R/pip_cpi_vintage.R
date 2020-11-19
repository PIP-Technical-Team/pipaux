#' Check CPI Vintage
#'
#' @param msrdir character: measure directory.
#' @param dlwdir character: Datalibweb directory
#' @param force logical: If TRUE force update of veintage level 1.
#'
#' @return
#' @export
#'
#' @examples
pip_cpi_vintage <- function(msrdir = paste0(getOption("pipaux.maindir"), "_aux/", measure, "/"),
                            dlwdir = getOption("pipaux.dlwdir"),
                            force  = FALSE) {

  time    <- format(Sys.time(), "%Y%m%d%H%M%S") # find a way to account for time zones
  measure <- "cpi"

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #---------   Prepar3 date   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # get directories
  cpi_files <- fs::dir_ls(dlwdir, regexp = "GMD_CPI\\.dta$", recurse = TRUE, type = "file")
  # load data
  cpi_list  <- purrr::map(.x = cpi_files,
                         .f = load_cpi)

  # create one single dataframe
  cp        <- data.table::rbindlist(cpi_list,
                                     use.names = TRUE,
                                     fill      = TRUE)

  #GEt vintage for each c("code", "year", "survname", "datalevel")
  byvars    <- c("code", "year", "survname", "datalevel")
  changevar <- "change_cpi2011"
  activevar <- "cpi2011"

  vintage <- cp[ # keep only 1s
    get(changevar) == 1
  ][,
    maxid := cpi_ppp_id == max(cpi_ppp_id),
    by = c(byvars)
  ][
    maxid == TRUE
  ][,
    .(unique(get(activevar))),
    by = c(byvars, "cpi_ppp_id")
  ]

  setnames(vintage, "V1", activevar)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #---------   check version and save  ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  # save file
  sfile <- paste0(msrdir, measure, "cpi_vintage.rds")

  equal_vintage <- TRUE
  if (fs::file_exists(sfile)) {

    cfile               <- readr::read_rds(sfile)
    attr(cfile, "time") <- NULL # remove attributes
    attr(cfile, "user") <- NULL # remove attributes
    equal_vintage       <- all.equal(cfile, vintage)

  } else {

    equal_vintage <- FALSE

  }

  if (equal_vintage == FALSE || force == TRUE) {

    attr(vintage, "time") <- time
    attr(vintage, "user") <- Sys.info()[8]

    readr::write_rds(x    = vintage,
                     file = sfile
    )
  }

  return(!equal_vintage)

} # end of vintage_level_2

#' Load cpi files and create CPI ID variable
#'
#' @param x character: cpi file name
#'
#' @return data frame
#'
#' @examples
load_cpi <- function(x) {
  cpi_ppp_id    <- gsub("(.*/Support_2005_)([^/]+)(_CPI\\.dta$)", "\\2", x)
  df            <- haven::read_dta(x)
  df$cpi_ppp_id <- cpi_ppp_id

  to_keep <- c("label")

  to_keep_regx <- paste(to_keep, collapse = "|")

  nn  <- names(df)
  for (x in seq_along(nn)) {

    ats       <- attributes(df[[x]])
    atsn      <- names(ats)
    to_remove <- atsn[!grepl(to_keep_regx, atsn)]

    for (i in seq_along(to_remove)) {
      attr(df[[x]], to_remove[i]) <- NULL
    }

  }


  return(df)
}
