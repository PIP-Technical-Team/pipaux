#' Title
#'
#' @param measure
#' @param msrdir
#' @param dlwdir
#' @param force
#'
#' @return
#' @export
#'
#' @examples
vintage_level_2 <- function(measure,
                            msrdir = paste0(getOption("pipaux.maindir"), "_aux/", measure, "/"),
                            dlwdir = getOption("pipaux.dlwdir"),
                            force  = FALSE) {

  time <- format(Sys.time(), "%Y%m%d%H%M%S") # find a way to account for time zones

  # get directories
  dirs <- dir(dlwdir)
  dirs <- grep("[0-9]{2}_[Mm]_[Vv][0-9]{2}", dirs, value = TRUE)
  dt       <- data.table(orig = dirs)

  # Breakdown ID into different variables
  cnames <-
    c(
      "country_code",
      "year",
      "survey_acronym",
      "vermast",
      "M",
      "veralt",
      "A",
      "collection"
    )

  cpi_avble <-
    dt[,
       # Name sections of filename into variables
       (cnames) := tstrsplit(orig, "_",
                             fixed=TRUE)
      ][
        !is.na(vermast) & !is.na(veralt)
        ]

  cpi_files <- paste0(dlwdir, cpi_avble$orig, "/", cpi_avble$orig, "_CPIICP.dta")

  cpi_list <- purrr::map(.x = cpi_files,
                         .f = load_cpi)

  cp <- data.table::rbindlist(cpi_list,
                              use.names = TRUE,
                              fill      = TRUE)


  if (measure == "cpi") {

    byvars    <- c("code", "year", "survname", "datalevel")
    changevar <- "change_cpi2011"
    activevar <- "cpi2011"

  } else {

    byvars    <- c("code", "datalevel")
    changevar <- "change_icp2011"
    acttivar  <- "icp2011"

  }

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


  # save file
  sfile <- paste0(msrdir, measure, "_vintage_level_2.rds")

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
  cpi_ppp_id    <- gsub("(.*/Support_2005_)([^/]+)(_CPIICP\\.dta$)", "\\2", x)
  df        <- haven::read_dta(x)
  df$cpi_ppp_id <- cpi_ppp_id
  return(df)
}
