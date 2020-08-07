#' update CPI dataframe
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

  pip_sign_save(x       = cpi,
                measure = "cpi",
                msrdir  = msrdir,
                force   = force)

}
