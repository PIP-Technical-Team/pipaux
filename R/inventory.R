# Auxiliary function that creates an inventory of value changes per country/year of the following measures:
# cpi, ppp, pfw, pop, gdp

# Step 1: load aux data of the current release
# Step 2: check for changes with an older version
# Step 3: write inventory, a dataframe
# Step 4: save inventory

inventory_aux <- function(measures     = c("cpi", "ppp", "gdp"),
                          maindir      = getOption("pipaux.working_dir"),
                          old_release,
                          save_to_file = TRUE) {

  # Get current release
  pipfun::get_wrk_release()

  release <- paste0(wrk_release$release,
                    "_",
                    wrk_release$identity)

  # Get vars to compare by
  byvars <- switch()

  # Construct directories path

  # old release
  #aux_dirpath_old <- fs::path(auxdata_dir,
  #                            old_release)

  # Load files

  current_files <- setNames(
    lapply(measures, function(x) {
      load_aux(measure = x,
               maindir = maindir,
               branch = release
      )
    }),
    measures
  )


  old_files <- setNames(
    lapply(measures, function(x) {
      load_aux(measure = x,
               maindir = maindir,
               branch = old_release
      )
    }),
    measures
  )

  # Compare with myrror
  myrror_report <- lapply(measures, function(x) {

    # Get vars to compare by
    byvars <- switch(x,
                     "cpi" = c("country_code", "year"),
                     c("country_code", "reporting_level"))

    myrror::myrror(current_files$x,
                   old_files$x)
  })


  names(myrror_report) <- measures




  # Write inventory data frame

  # save it

}
