
#' Update CPI
#'
#' @inheritParams pip_prices
#' @keywords internal
pip_cpi_update <- function(maindir = gls$PIP_DATA_DIR,
                           dlwdir  = Sys.getenv("PIP_DLW_ROOT_DIR"),
                           force = FALSE) {
  measure <- "cpi"
  msrdir <- paste0(maindir, "_aux/", measure, "/") # measure dir
  cl <- pip_country_list("load", maindir = maindir)
  setDT(cl)

  vintage <- FALSE
  vintage <- pip_cpi_vintage(
    msrdir = msrdir,
    dlwdir = dlwdir,
    force = force
  )


  if (vintage == TRUE || force == TRUE) {
    cpi_files <- fs::dir_ls(dlwdir,
                            regexp = "GMD_CPI\\.dta$",
                            recurse = TRUE,
                            type = "file"
    )

    latest_cpi <- max(cpi_files)
    cpi_id <- gsub("(.*/Support_2005_)([^/]+)(_GMD_CPI\\.dta$)", "\\2", latest_cpi)

    # Read latest dataset from file
    cpidlw <- haven::read_dta(latest_cpi)

    # Clean data
    cpi <- pip_cpi_clean(cpidlw, cpi_id = cpi_id)

    # Remove any non-WDI countries
    cpi <- cpi[country_code %in% cl$country_code]

    # Save
    pip_sign_save(
      x = cpi,
      measure = "cpi",
      msrdir = msrdir,
      force = force
    )
  } else {
    cli::cli_alert_success("CPI data is up to date")
    return(invisible(FALSE))
  }
}


#' Clean CPI data
#'
#' Clean CPI data from Datalibweb to meet PIP protocols.
#'
#' @param y dataset with CPI data from datalibweb. loaded in `pip_prices()`.
#' @param cpi_id character: CPI ID. Extracted from `pip_cpi_update()`
#' @param cpivar character: CPI variable to be used as default. Currently it is
#' "cpi2011".
#'
#' @keywords internal
cpi_clean <- function(y,
                          cpi_id,
                          cpivar = getOption("pipaux.cpivar")) {
  x <- data.table::as.data.table(y)

  # vars to keep
  keep_vars <- c(
    "country_code", "cpi_year", "survey_year",
    "cpi", "ccf", "survey_acronym", "change_cpi2011",
    grep("^cpi", names(x), value = TRUE), "cpi_id"
  )

  # modifications to the database
  x[
    ,
    c("cur_adj", "ccf")
    := {
      cur_adj <- ifelse(is.na(cur_adj), 1, cur_adj)
      ccf <- 1 / cur_adj

      list(cur_adj, ccf)
    }
  ][
    ,
    `:=`(
      country_code   = code,
      cpi_year       = as.integer(year),
      survey_year    = ref_year,
      cpi            = get(cpivar),
      survey_acronym = survname,
      cpi_id         = (cpi_id),
      cpi_domain     = as.character(cpi_domain),
      cpi_data_level = as.character(cpi_data_level)
    )
  ][
    ,
    # This part should not exist if the raw data
    # had been created properly
    cpi_data_level := fcase(
      cpi_domain %chin% c("urban/rural", "2") & cpi_data_level == "0", "rural",
      cpi_domain %chin% c("urban/rural", "2") & cpi_data_level == "1", "urban",
      cpi_domain %chin% c("national", "1") & cpi_data_level %chin% c("2", "", NA_character_),
      "national",
      default = ""
    )
  ]
  # keep final vars
  x <- x[
    ,
    ..keep_vars
  ]

  x <- unique(x) # remove duplicates
  return(x)
}



#' Check CPI Vintage
#'
#' @param msrdir character: measure directory.
#' @param dlwdir character: Datalibweb directory
#' @param force logical: If TRUE force update of veintage level 1.
#'
#' @keywords internal
cpi_vintage <- function(msrdir = paste0(gls$PIP_DATA_DIR, "_aux/", measure, "/"),
                        dlwdir  = Sys.getenv("PIP_DLW_ROOT_DIR"),
                        force = FALSE) {
  time <- format(Sys.time(), "%Y%m%d%H%M%S") # find a way to account for time zones
  measure <- "cpi"

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #---------   Prepar3 date   ---------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # get directories
  cpi_files <- fs::dir_ls(dlwdir, regexp = "GMD_CPI\\.dta$", recurse = TRUE, type = "file")
  # load data
  cpi_list <- purrr::map(
    .x = cpi_files,
    .f = load_cpi
  )

  # create one single dataframe
  cp <- data.table::rbindlist(cpi_list,
                              use.names = TRUE,
                              fill      = TRUE
  )

  # GEt vintage for each c("code", "year", "survname", "datalevel")
  byvars <- c("code", "year", "survname", "datalevel")
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

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #---------   check version and save  ---------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  # save file
  sfile <- paste0(msrdir, measure, "cpi_vintage.rds")

  equal_vintage <- TRUE
  if (fs::file_exists(sfile)) {
    cfile <- readr::read_rds(sfile)
    attr(cfile, "time") <- NULL # remove attributes
    attr(cfile, "user") <- NULL # remove attributes
    cf_vt <- all.equal(cfile, vintage)

    if (inherits(cf_vt, "character")) {
      equal_vintage <- FALSE
    }
  } else {
    equal_vintage <- FALSE
  }

  if (equal_vintage == FALSE || force == TRUE) {
    attr(vintage, "time") <- time
    attr(vintage, "user") <- Sys.info()[8]

    readr::write_rds(
      x = vintage,
      file = sfile
    )
  }

  return(!equal_vintage)
} # end of vintage_level_2

#' Load cpi files and create CPI ID variable
#' @param x character: cpi file name
#' @return data frame
load_cpi <- function(x) {
  cpi_ppp_id <- gsub("(.*/Support_2005_)([^/]+)(_CPI\\.dta$)", "\\2", x)
  df <- haven::read_dta(x)
  df$cpi_ppp_id <- cpi_ppp_id

  to_keep <- c("label")

  to_keep_regx <- paste(to_keep, collapse = "|")

  nn <- names(df)
  for (x in seq_along(nn)) {
    ats <- attributes(df[[x]])
    atsn <- names(ats)
    to_remove <- atsn[!grepl(to_keep_regx, atsn)]

    for (i in seq_along(to_remove)) {
      attr(df[[x]], to_remove[i]) <- NULL
    }
  }


  return(df)
}
