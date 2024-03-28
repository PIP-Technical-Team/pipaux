#' Update National accounts data from WDI
#'
#' GDP and HFCE data from WDI. It could be either from API or from file
#'
#' @param detail has an option TRUE/FALSE, default value is FALSE
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
                           from    = c("gh", "file", "api"),
                           detail  = getOption("pipaux.detail.raw")) {


  from   <- match.arg(from)
  branch <- match.arg(branch)

  #   ______________________________________________________
  #   Computations                                    ####
  measure <- "wdi"

  ##  ...............................................................
  ##  From file                                          ####

  if (from   %in% c("file", "gh")) {
    wdi <- pipfun::load_from_gh(measure = measure,
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
  # validate wdi raw data
  wdi_validate_raw(wdi = wdi, detail = detail)

  #   _________________________________________________________________________
  #   Save and Return                                                     ####

  if (branch == "main") {
    branch <- ""
  }
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

