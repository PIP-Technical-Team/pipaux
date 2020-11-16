#' Check  if vintage level 1 has not changed or update it in case it has
#' changed.
#'
#' @param measure character: Measure to check, either cpi of ppp
#' @param msrdir character: measure directory.
#' @param dlwdir character: Datalibweb directory
#' @param force logical: If TRUE force update of veintage level 1.
#'
#' @return
#' @export
#'
#' @examples
vintage_level_1 <- function(measure,
                            msrdir = paste0(getOption("pipaux.maindir"), "_aux/", measure, "/"),
                            dlwdir = getOption("pipaux.dlwdir"),
                            force  = FALSE) {

  # Date  and time
  time <- format(Sys.time(), "%Y%m%d%H%M%S") # find a way to account for time zones

  dirs <- dir(dlwdir)
  dirs <- grep("[0-9]{2}_[Mm]_[Vv][0-9]{2}", dirs, value = TRUE)

  sfile <- paste0(msrdir, measure, "_vintage_level_1.rds")

  equal_vintage <- TRUE
  if (fs::file_exists(sfile)) {
    cfile             <- readr::read_rds(sfile)
    attributes(cfile) <- NULL # remove attributes
    equal_vintage     <- identical(cfile, dirs)
  } else {
    equal_vintage <- FALSE
  }

  if (equal_vintage == FALSE || force == TRUE) {
    attr(dirs, "time") <- time
    attr(dirs, "user") <- Sys.info()[8]

    readr::write_rds(x    = dirs,
                    file = sfile
    )
  }

  return(!equal_vintage)

}
