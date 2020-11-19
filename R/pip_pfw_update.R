#' update CPI dataframe
#'
#' @param msrdir character: measure (PFW) directory. created on `pip_prices()`.
#' @inheritParams pip_prices
#'
#' @return
#' @export
#'
#' @examples
pip_pfw_update <- function(msrdir = paste0(getOption("pipaux.maindir"), "_aux/pfw/"),
                           dlwdir = getOption("pipaux.dlwdir"),
                           force  = FALSE){

  # check for last version in dlw

  pfw_files  <- fs::dir_ls(dlwdir,
                           regexp = "Survey_price_framework\\.dta$",
                           recurse = TRUE,
                           type = "file")

  latest_pfw <- max(pfw_files)
  pfw_id <- gsub("(.*/Support_2005_)([^/]+)(/Data.*)", "\\2", latest_pfw)

  pfwdlw     <- haven::read_dta(latest_pfw)
  pfw        <- pip_pfw_clean(pfwdlw, pfw_id = pfw_id)

  pip_sign_save(x       = pfw,
                measure = "pfw",
                msrdir  = msrdir,
                force   = force)

}
