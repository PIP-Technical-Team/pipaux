#' Update National accounts data from WDI
#'
#' GDP and HFCE data from WDI. It could be either from API or from file
#'
#' @inheritParams pip_gdp
#' @return data.table with gdp and pce variables
#' @export
#'
#' @examples
#' pip_wdi_update()
pip_wdi_update <- function(force   = FALSE,
                           maindir = gls$PIP_DATA_DIR,
                           owner   = getOption("pipfun.ghowner"),
                           branch  = c("DEV", "PROD", "main"),
                           tag     = match.arg(branch),
                           from    = c("gh", "file", "api")) {


  from   <- match.arg(from)
  branch <- match.arg(branch)

  #   _________________________________________________________________
  #   on.exit                                                 ####
  on.exit({

  })

  #   ____________________________________________________________________
  #   Defenses                                        ####
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

  if (from   %in% c("file", "gh")) {
    wdi <- load_raw_aux(measure = measure,
                        owner = owner,
                        branch = branch)

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

  msrdir <- fs::path(maindir, "_aux", branch, measure) # measure dir
  saved <- pipfun::pip_sign_save(
    x       = wdi,
    measure = measure,
    msrdir  = msrdir,
    force   = force,
    save_dta = FALSE
  )

  return(invisible(saved))

}

