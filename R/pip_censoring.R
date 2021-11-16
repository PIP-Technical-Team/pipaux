#' Censoring data
#'
#' Load or update censoring data
#'
#' @inheritParams pip_prices
#' @export
#' @import data.table
pip_censoring  <- function(action = "update",
                           force = FALSE,
                           maindir = gls$PIP_DATA_DIR) {
  measure <- "censoring"
  msrdir <- paste0(maindir, "_aux/", measure, "/") # measure dir

  if (action == "update") {

    sheets <- readxl::excel_sheets(paste0(msrdir, "censored.xlsx"))
    dl <- vector("list", 2)
    for (i in seq_along(sheets)) {
      dl[[i]] <- readxl::read_xlsx(paste0(msrdir, "censored.xlsx"), sheet = sheets[i])
    }
    names(dl) <- sheets

    # Add id columns
    dl$countries$id <-
      with(dl$countries,
           sprintf(
             "%s_%s_%s_%s_%s",
             country_code, reporting_year,
             survey_acronym, welfare_type,
             reporting_level
           ))
    dl$regions$id <-
      with(dl$regions,
           sprintf('%s_%s',
                   region_code, reporting_year
           ))

    # Check hash
    hash <- digest::digest(dl, algo = "xxhash64")
    current_hash <- tryCatch(
      readr::read_lines(paste0(msrdir, "_datasignature.txt")),
      error = function(e) NULL
    )

    # Save data
    if (hash != current_hash || force || is.null(current_hash)) {

      # Create vintage folder
      wholedir <- paste0(msrdir, "_vintage/")
      if (!(dir.exists(wholedir))) {
        dir.create(wholedir, recursive = TRUE)
      }

      # Write files
      time <- format(Sys.time(), "%Y%m%d%H%M%S")
      saveRDS(dl, paste0(maindir, "_aux/censoring/censoring.rds"))
      saveRDS(dl, sprintf("%s_vintage/censoring_%s.rds", msrdir, time))
      readr::write_lines(hash, file = paste0(maindir, "_aux/censoring/_datasignature.txt"))

      # Print msg
      infmsg <- paste(
        "Data signature has changed, it was not found,",
        "or update was forced.\n",
        paste0("`", "censoring", ".rds` has been updated")
      )
      rlang::inform(infmsg)
      return(invisible(TRUE))
    } else {
      rlang::inform("Data signature is up to date.\nNo update performed")
      return(invisible(FALSE))
    }

  } else if (action == "load") {
    df <- load_aux(
      maindir = maindir,
      measure = measure
    )
    return(df)
  } else {
    rlang::abort(c("`action` must be `update` or `load`",
                   x = paste0("you provided `", action, "`")
    ))
  }
}
