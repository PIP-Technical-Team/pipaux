#' Data signature of PIP auxiliary data
#'
#' @param x Data frame to be signed and saved
#' @param measure type of data frame so it could be saved properly
#' @param msrdir  directory where data signature would be saved.
#' Created in `pip_prices()`
#' @param force logical: If TRUE data will be overwritten.
#'
#' @export
pip_sign_save <- function(x,
                          measure,
                          msrdir = paste0(getOption("pipaux.maindir"), "_aux/", measure, "/"),
                          force) {

  # Note: clean CPI data file and then create data signature
  ds_dlw <- digest::digest(x,  algo = "xxhash64") # Data signature of file

  # check signature of current fst file
  ds_production_path <- paste0(msrdir, measure, "_datasignature.txt")  # data signature in production


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
    fst::write_fst(x = x,
                   path = paste0(msrdir, measure, ".fst")
                  )

    haven::write_dta(data = x,
                     path = paste0(msrdir, measure, ".dta")
                    )
    fst::write_fst(x = x,
                   path = paste0(msrdir, "_vintage/", measure, "_", time,".fst")
    )
    haven::write_dta(data = x,
                     path = paste0(msrdir, "_vintage/", measure, "_", time,".dta")
    )

    ds_text <- c(ds_dlw, time, Sys.info()[8])

    readr::write_lines(x    = ds_dlw,
                       file = ds_production_path)

    infmsg <- paste("Data signature has changed, it was not found,",
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

