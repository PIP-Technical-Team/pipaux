# Auxiliary function that creates an inventory of value changes per country/year of the following measures:
# cpi, ppp, pfw, pop, gdp

# Step 1: load aux data of the current release
# Step 2: check for changes with an older version
# Step 3: write inventory, a list with one element per measure
# Step 4: save inventory

get_aux_changes <- function(measure      = "cpi",
                            maindir      = getOption("pipaux.working_dir"),
                            old_release  = NULL,
                            key_cols     = getOption("pipaux.key_vars"),
                            #ext          = c("qs", "csv"),
                            verbose      = TRUE) {

  # _______________________________________#
  # Get arguments ####

  #ext <- match.arg(ext)

  if (is.null(old_release)) {
    cli::cli_abort("Release to compare with must be provided, specifying date and identity.")
    # to do: take last available release
  }

  # Get current release ####

  pipfun::get_wrk_release()

  release <- paste0(wrk_release$release,
                    "_",
                    wrk_release$identity)


  # _______________________________________#
  # Load files with error handling ####

  new_df <- tryCatch({

    load_aux(measure = measure,
             maindir = maindir,
             branch  = release)
  },

  error = function(e) {
    cli::cli_alert_danger("Failed to load data for {.strong {measure}} in current release {.strong {release}}")
    stop(e)
  })

  old_df <- tryCatch({

    load_aux(measure = measure,
             maindir = maindir,
             branch  = old_release)
  },

  error = function(e) {
    cli::cli_alert_warning(
      "Failed to load OLD data for {.strong {measure}} in release {.strong {old_release}}.
       Comparison will be skipped.")

    return(NULL)
  })

  if (is.null(old_df)) {
    cli::cli_alert_warning(
      "Empty OLD data for {.strong {measure}} in release {.strong {old_release}}.
       Comparison will be skipped.")
    return(invisible(NULL))
  }


  # Key vars to compare by --- #

  key_cols <- intersect(key_cols,
                        names(new_df))

  # _______________________________________#
  # Extract differences ####

  # Run comparison
  myr_obj <- myrror::myrror(
              dfx                 = new_df,
              dfy                 = old_df,
              by                  = key_cols, # keys for matching (e.g., country and year)
              compare_type        = FALSE,
              compare_values      = TRUE,
              extract_diff_values = TRUE,
              interactive         = FALSE)

  # diff_list <- myrror::extract_diff_values(myrror_object = myr_obj,
  #                                          output        = "simple")

  # Extract differences in table format
  diff_table <- myrror::extract_diff_table(myrror_object = myr_obj,
                                           by            = key_cols,
                                           output        = "simple",
                                           interactive   = FALSE)

  # Add metadata: files paths, measure

  new_path <- fs::path(maindir,
                       "aux_data",
                       release,
                       measure,
                       paste0(measure, ".", "qs"))

  old_path <- fs::path(maindir,
                       "aux_data",
                       old_release,
                       measure,
                       paste0(measure, ".", "qs"))

  diff_table <- diff_table |>
    fmutate(measure = measure,
            path.x  = new_path,
            path.y  = old_path)

  # _______________________________________#
  # Return ####

  return(diff_table)

}

inventory_aux_changes <- function(measure     = NULL,
                                  maindir     = getOption("pipaux.working_dir"),
                                  owner       = "PIP-Technical-Team",
                                  old_release = NULL,
                                  verbose     = FALSE,
                                  ...) {

  # _______________________________________#
  # Get arguments ####

  # Get all measures from repositories under PIP-Technical-Team that start with "aux_"

  all_measures <- gh::gh("GET /users/{username}/repos",
                         username = owner) |>
    vapply("[[", "", "name") |>
    grep("^aux_", x = _, value = TRUE) |>
    (\(x) sub("^aux_", "", x))()

  if (!is.null(measure)) {
    all_measures <- all_measures[all_measures %in% measure]
  }

  # _______________________________________#
  # Extract changes ####

  # Initialize result list

  res <- setNames(

    lapply(all_measures, \(x) {

      get_aux_changes(
        measure     = x,
        maindir     = maindir,
        old_release = old_release,
        #key_cols    = key_cols,
        verbose     = verbose
      )

       }),

    all_measures
  )

  # _______________________________________#
  # Return ####




}
