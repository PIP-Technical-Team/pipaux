#' update CPI dataframe
#'
#' @param msrdir character: measure (CPI) directory. created on `pip_prices()`.
#' @param dlwdir character: datalibweb directory. available in `pip_prices()`
#' @param force  logical: if TRUE force update of CPI data
#'
#' @return
#' @export
#'
#' @examples
pip_pfw_update <- function(msrdir, dlwdir, force){

  # check for last version in dlw
  dlwdir_l   <- latest_dlw_dir(dlwdir = dlwdir) # from utils.R
  pfwdlw_dir <- paste0(dlwdir, dlwdir_l,"/Data/Stata/Survey_price_framework.dta")

  pfwdlw     <- haven::read_dta(pfwdlw_dir)
  pfw        <- pip_pfw_clean(pfwdlw)

  pip_sign_save(x       = pfw,
                measure = "pfw",
                msrdir  = msrdir,
                force   = force)

}
