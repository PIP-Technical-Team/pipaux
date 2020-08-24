pip_update_all_aux <- function(force = FALSE) {
  pip_pfw(force = force)
  pip_cpi(force = force)
  pip_gdp(force = force)
  pip_pce(force = force)
  pip_pop(force = force)
  pip_ppp(force = force)
  pip_maddison(force = force)
  pip_country_list(force = force)

  return(invisible())
}
