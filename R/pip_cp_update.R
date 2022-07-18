#' Update Country Profiles
#'
#' Update a list with country profiles data
#'
#' @inheritParams pip_prices
#' @keywords internal
pip_cp_update <- function(maindir = gls$PIP_DATA_DIR,
                          force = FALSE,
                          owner   = getOption("pipaux.ghowner"),
                          branch  = c("DEV", "PROD", "main"),
                          tag     = match.arg(branch)) {

  measure <- "cp"
  branch  <- match.arg(branch)

  file_names <-
    c(
      "indicator_values_country_chart4",
      "indicator_values_country_KI1",
      "indicator_values_country_chart1_chart2_KI2",
      "indicator_values_country_chart5",
      "indicator_values_country_chart3",
      "indicator_values_country_chart6_KI4",
      "indicator_values_country_KI5_KI6_KI7"
    )


  raw_files <- purrr::map(.x = file_names,
                          .f = ~{
                            load_raw_aux(measure = "cp",
                                         owner  = owner,
                                         branch = branch,
                                         filename = .x)
                          })


  dl <- pip_cp_clean(raw_files,
                     file_names)

  msrdir <- fs::path(maindir, "_aux", branch, measure) # measure dir

  saved <- pip_sign_save(
    x       = dl,
    measure = measure,
    msrdir  = msrdir,
    force   = force
  )

  return(invisible(saved))
}
