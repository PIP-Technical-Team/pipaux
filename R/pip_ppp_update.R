#' Update PPP data frame
#'
#' @param msrdir character: measure (PPP) directory. created on `pip_prices()`.
#' @inheritParams pip_prices
#'
#' @return
#' @export
#'
#' @examples
pip_ppp_update <- function(msrdir = paste0(getOption("pipaux.maindir"), "_aux/ppp/"),
                           dlwdir = getOption("pipaux.dlwdir"),
                           force  = FALSE){

  ppp_files  <- fs::dir_ls(dlwdir,
                           regexp = "pppdata_allvintages\\.dta$",
                           recurse = TRUE,
                           type = "file")

  latest_ppp <- max(ppp_files)

  # check for last version in dlw
  pppdlw     <- haven::read_dta(latest_ppp)
  ppp        <- pip_ppp_clean(pppdlw)
  pip_sign_save(x       = ppp,
                measure = "ppp",
                msrdir  = msrdir,
                force   = force)

}
