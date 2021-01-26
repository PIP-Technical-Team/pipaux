#' List of countries
#'
#' Load or update dataset with WDI countries.
#'
#' @inheritParams pip_prices
#' @export
pip_country_list <- function(action = "update",
                             force  = FALSE,
                             maindir  = getOption("pipaux.maindir")) {

  measure <- "country_list"
  msrdir  <- paste0(maindir, "_aux/", measure, "/")  # measure dir

  if (action == "update") {

    country_list <- wbstats::wb_countries()
    setDT(country_list)
    country_list <- country_list[region != 'Aggregates']

    # Recode names
    setnames(country_list,
             old = c("iso3c"),
             new = c("country_code")
    )

    # Keep relevant variables
    country_list <-
      country_list[,
                   .(country_code, country, region, income_level, lending_type)
      ]

    # country_list <-
    #   suppressMessages(
    #     readr::read_csv(
    #       paste0(msrdir, measure, ".csv")
    #     )
    #   )

    pip_sign_save(x       = country_list,
                  measure = "country_list",
                  msrdir  = msrdir,
                  force   = force)

  } else if (action == "load") {
    df <- load_aux(maindir  = maindir,
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
