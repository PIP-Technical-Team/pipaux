#' update CPI dataframe
#'
#' @param msrdir character: measure (CPI) directory. created on `pip_prices()`.
#' @inheritParams pip_prices
#'
#' @return
#' @export
#'
#' @examples
pip_cpi_update <- function(msrdir = paste0(getOption("pipaux.maindir"), "_aux/cpi/"),
                           dlwdir = getOption("pipaux.dlwdir"),
                           force  = FALSE){

  # check for last version in dlw
  change_vintage1   <- vintage_level_1("cpi",
                                msrdir = msrdir,
                                dlwdir = dlwdir,
                                force  =  force)

  if (change_vintage1 == TRUE || force == TRUE) {

    change_vintage2   <- vintage_level_2("cpi",
                                  msrdir = msrdir,
                                  dlwdir = dlwdir,
                                  force  =  force)
  } else {
    change_vintage2 <- FALSE
  }


  if (change_vintage2 == TRUE || force == TRUE) {

    dlwdir_l   <- latest_dlw_dir(dlwdir = dlwdir) # from utils.R
    cpidlw_dir <- paste0(dlwdir, dlwdir_l, "/", dlwdir_l, "_CPIICP.dta")

    cpidlw     <- haven::read_dta(cpidlw_dir)
    cpi        <- pip_cpi_clean(cpidlw, cpi_id = dlwdir_l)

    pip_sign_save(x       = cpi,
                  measure = "cpi",
                  msrdir  = msrdir,
                  force   = force)
  } else {
    cli::cli_alert_success("cpi data is up to date")
  }

}

