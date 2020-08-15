pip_country_list <- function(action = "update",
                             force  = FALSE) {

  measure <- "country_list"
  r       <- pip_aux_values()
  msrdir  <- paste0(r$maindir, "_aux/", measure, "/")  # measure dir

  if (action == "update") {

    country_list <-
      suppressMessages(
        readr::read_csv(
          paste0(msrdir, measure, ".csv")
          )
      )

    pip_sign_save(x       = country_list,
                  measure = "country_list",
                  msrdir  = msrdir,
                  force   = force)

  } else if (action == "load") {
    df <- pip_aux_load(msrdir  = msrdir,
                       measure = measure)
    return(df)

  } else {
    msg <- paste("action `", action,"` is not a valid action.")
    rlang::abort(c(
                  msg,
                  i = "make sure you selected `update` or `load`"
                  ),
              class = "pipaux_error"
              )
  }

}
