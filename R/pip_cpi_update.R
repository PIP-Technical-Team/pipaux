#' Title
#'
#' @param msrdir
#' @param dlwdir
#' @param force
#'
#' @return
#' @export
#'
#' @examples
pip_cpi_update <- function(msrdir, dlwdir, force){

  # check for last version in dlw
  dlwdir_l   <- latest_dlw_dir(dlwdir = dlwdir) # from utils.R
  cpidlw_dir <- paste0(dlwdir, dlwdir_l,"/Data/Stata/Final_CPI_PPP_to_be_used.dta")

  cpidlw     <- haven::read_dta(cpidlw_dir)
  cpi        <- pip_cpi_clean(cpidlw)

  # Note: clean CPI data file and then create datasignature
  ds_dlw <- digest::digest(cpi,  algo = "xxhash64") # Data signature of file

  # check signature of current fst file
  ds_production_path <- paste0(msrdir, "cpi_datasignature.txt")  # data signature in production


  if (file.exists(ds_production_path)) {

    # read data signature in production
    ds_production <- readr::read_lines(ds_production_path)[[1]]

  } else {

    # fake signature
    ds_production <- "0000"
  }

  #--------- if Signature from dlw is different from the one in production ---------

  if (ds_dlw != ds_production || force == TRUE) {

    # re-write cpi in production if data signature is not found
    fst::write_fst(x = cpi,
                   path = paste0(msrdir, "cpi.fst")
    )
    # Vintage
    time <- format(Sys.time(), "%Y%m%d%H%M%S") # find a way to account for time zones
    fst::write_fst(x = cpi,
                   path = paste0(msrdir, "_vintage/cpi_", time,".fst")
    )

    ds_text <- c(ds_dlw, time, Sys.info()[8])

    readr::write_lines(x = ds_dlw,
                       path = ds_production_path)

    infmsg <- paste("Data signature has changed, it was not found,",
                    "or update was forced.\n",
                    "`cpi.fst` has been updated")
    rlang::inform(infmsg)
    return(invisible(TRUE))
  } else {
    rlang::inform("Data signature is up to date.\nNo update performed")
    return(invisible(FALSE))
  }
}

