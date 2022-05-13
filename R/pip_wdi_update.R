#' Update National accounts data from WDI
#'
#' GDP and HFCE data from WDI. It could be either from API or from file
#'
#' @inheritParams pip_gdp
#' @param from character: Either "file" or "api". Default is file.
#'
#' @return data.table with gdp and pce variables
#' @export
#'
#' @examples
#' pip_wdi_update()
pip_wdi_update <- function(force = FALSE,
                           maindir = gls$PIP_DATA_DIR,
                           from = c("file", "api")) {

  #   ____________________________________________________________________________
  #   on.exit                                                                 ####
  on.exit({

  })

  #   ____________________________________________________________________________
  #   Defenses                                                                ####
  from <- match.arg(from)
  stopifnot( exprs = {

  }
  )

  #   _______________________________________________________________________
  #   Early returns                                                 ####

  if (FALSE) {
    return()
  }

  #   _______________________________________________________________________
  #   Computations                                                    ####
  measure <- "wdi"
  msrdir <- fs::path(maindir, "_aux", measure)


  ##  .......................................................................
  ##  From file                                                         ####

  if (from == "file") {
    wdifile <- fs::path(msrdir, "WDIEXCEL.xlsx")

    # convert dots and spaces to _
    name_repair <- function(nms) {
      gsub("[.[:space:]]", "_", nms) |>
      tolower()
    }

    orig    <- readxl::read_xlsx(wdifile, sheet = "Data",
                                 .name_repair = name_repair)

    setDT(orig)

    orig <- orig[indicator_code  %in% c("NY.GDP.PCAP.KD", "NE.CON.PRVT.PC.KD")
                 ][,
                   c("indicator_name", "country_name") := NULL]

    df   <- melt(orig,
                 id.vars         = c("country_code", "indicator_code"),
                 variable.name   = "year",
                 variable.factor = FALSE,
                 value.factor    = FALSE
                 )

    wdi   <- dcast(df,
                  country_code + year ~indicator_code)
    wdi[,
        year := as.numeric(year)]

  } else {
  ##  ........................................................................
  ##  From API                                                            ####
    wdi_indicators <- c("NY.GDP.PCAP.KD", "NE.CON.PRVT.PC.KD")
    wdi   <- wbstats::wb_data(indicator = wdi_indicators,
                               lang = "en") |>
      setDT()

    wdi[,
        c("country", "iso2c") := NULL]

    # Rename columns
    setnames(wdi,
             old = c("iso3c", "date"),
             new = c("country_code", "year")
    )
  }

  #   _________________________________________________________________________
  #   Save and Return                                                     ####
  pip_sign_save(
    x        = wdi,
    measure  = measure,
    msrdir   = msrdir,
    force    = force,
    save_dta = FALSE
  )

}

