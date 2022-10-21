#' Save PIP auxiliary data
#'
#' @description `r lifecycle::badge("superseded")`
#'
#' This function is deprecated because of the new, more flexible and general
#' function `pipfun::pip_sign_save()`
#' @param x data.frame Data frame to be signed and saved.
#' @inheritParams pip_prices
#' @param msrdir character: Directory where the data and data signature will be
#'   saved.
#' @param save_dta logical: If TRUE a Stata (.dta) version of the dataset is
#'   also saved.
#' @keywords internal
#' @return logical
pip_sign_save <- function(x,
                          measure,
                          msrdir,
                          force = FALSE,
                          save_dta = TRUE) {

  lifecycle::deprecate_warn("0.1.0.9002",
                            "pip_sign_save()",
                            "pipfun::pip_sign_save()")

#   ____________________________________________________________________________
#   Files and directories                                                   ####

  wholedir <- fs::path(msrdir, "_vintage")
  if (!(fs::dir_exists(wholedir))) {
    fs::dir_create(wholedir, recurse = TRUE)
  }

  # check signature of current fst file
  ds_production_path <- fs::path(msrdir, paste0(measure, "_datasignature.txt")) # data signature in production

  if (fs::file_exists(ds_production_path)) {

    # read data signature in production
    ds_production <- readr::read_lines(ds_production_path)[[1]]
  } else {

    # fake signature
    ds_production <- "0000"
    ms_status <- "new"
  }


#   __________________________________________________________________
#   Check signature                                         ####

  #--------- if Signature from dlw is different from the one in production ---------

  # Note: clean CPI data file and then create data signature
  ds_dlw <- digest::digest(x, algo = "xxhash64") # Data signature of file

  if (ds_dlw != ds_production) {
    ms_status <- "changed"
  } else {
    ms_status <- "unchanged"
  }

  if (force == TRUE) {
    ms_status <- "forced"
  }


#   ____________________________________________________________________________
#   if signature changes or force = TRUE                                    ####

  if (ms_status %in% c("forced", "changed")) {

    # re-write x in production if data signature is not found
    # Vintage
    time <- format(Sys.time(), "%Y%m%d%H%M%S") # find a way to account for time zones

    attr(x, "datetime") <- time


##  ............................................................................
##  Save main file                                                          ####

    var_class <- purrr::map(x, class) # variables class

    if (is.data.frame(x) && !("list"  %in% unique(var_class))) {
      fst::write_fst(
        x = x,
        path = fs::path(msrdir, measure, ext = "fst")
        )

      if (save_dta) {
        haven::write_dta(
          data = x,
          path = fs::path(msrdir, measure, ext = "dta")
        )
      }
      ext <- "fst"
    } else {
      readr::write_rds(x = x,
                       file = fs::path(msrdir, measure, ext = "rds"))
      ext <- "rds"
    }

    qs::qsave(
      x = x,
      file = fs::path(msrdir, measure, ext = "qs")
    )

##  ............................................................................
##  Save vintages                                                           ####

    if (is.data.frame(x) && !("list"  %in% unique(var_class))) {
      fst::write_fst(
        x = x,
        path = fs::path(msrdir, "_vintage/",
                        paste0(measure, "_", time),  ext = "fst")
        )

      if (save_dta) {
        haven::write_dta(
          data = x,
          path = fs::path(msrdir, "_vintage/",
                          paste0(measure, "_", time),  ext = "dta"))
      }

    } else {

      readr::write_rds(x = x,
                       file = fs::path(msrdir, "_vintage/",
                                       paste0(measure, "_", time),
                                       ext = "rds"))

    }

    qs::qsave(
      x = x,
      file = fs::path(msrdir, "_vintage/", paste0(measure, "_", time),  ext = "qs")
    )


#   ____________________________________________________________________________
#   Signatures                                                              ####


    ds_text <- c(ds_dlw, time, Sys.info()[8])

    readr::write_lines(
      x = ds_dlw,
      file = ds_production_path
    )


    fillintext <- fcase(ms_status == "new", "was not found",
          ms_status == "forced", "has been changed forcefully",
          ms_status == "changed", "has changed",
          default = "")


    infmsg <-
      "Data signature {fillintext}
      {.file {measure}.{ext}} has been updated"

    cli::cli_alert_warning(infmsg)

    return(invisible(TRUE))

  } else {

    cli::cli_alert_info("Data signature is up to date.
                        {cli::col_blue('No update performed')}")
    return(invisible(FALSE))
  }
}
