#' Save PIP auxiliary data
#'
#' Save PIP auxiliary data with data signature.

#' @param x data.frame Data frame to be signed and saved.
#' @inheritParams pip_prices
#' @param msrdir character: Directory where the data and data signature will be saved.
#' @param save_dta logical: If TRUE a Stata (.dta) version of the dataset is also saved.
#' @keywords internal
pip_sign_save <- function(x,
                          measure,
                          msrdir,
                          force,
                          save_dta = TRUE) {

  # Note: clean CPI data file and then create data signature
  ds_dlw <- digest::digest(x, algo = "xxhash64") # Data signature of file

  # check signature of current fst file
  ds_production_path <- paste0(msrdir, measure, "_datasignature.txt") # data signature in production

  if (file.exists(ds_production_path)) {

    # read data signature in production
    ds_production <- readr::read_lines(ds_production_path)[[1]]
  } else {

    # fake signature
    ds_production <- "0000"
  }

  #--------- if Signature from dlw is different from the one in production ---------

  if (ds_dlw != ds_production || force == TRUE) {

    # make sure directory exists
    wholedir <- paste0(msrdir, "_vintage/")
    if (!(dir.exists(wholedir))) {
      dir.create(wholedir, recursive = TRUE)
    }

    # re-write x in production if data signature is not found
    # Vintage
    time <- format(Sys.time(), "%Y%m%d%H%M%S") # find a way to account for time zones

    attr(x, "datetime") <- time
    fst::write_fst(
      x = x,
      path = paste0(msrdir, measure, ".fst")
    )
    if (save_dta) {
      haven::write_dta(
        data = x,
        path = paste0(msrdir, measure, ".dta")
      )
    }

    fst::write_fst(
      x = x,
      path = paste0(msrdir, "_vintage/", measure, "_", time, ".fst")
    )
    if (save_dta) {
      haven::write_dta(
        data = x,
        path = paste0(msrdir, "_vintage/", measure, "_", time, ".dta")
      )
    }


    ds_text <- c(ds_dlw, time, Sys.info()[8])

    readr::write_lines(
      x = ds_dlw,
      file = ds_production_path
    )

    infmsg <- paste(
      "Data signature has changed, it was not found,",
      "or update was forced.\n",
      paste0("`", measure, ".fst` has been updated")
    )

    rlang::inform(infmsg)
    return(invisible(TRUE))
  } else {
    rlang::inform("Data signature is up to date.\nNo update performed")
    return(invisible(FALSE))
  }
}
