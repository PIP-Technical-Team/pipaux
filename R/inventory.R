# Auxiliary function that creates an inventory of value changes per country/year of the following measures:
# cpi, ppp, pfw, pop, gdp

# Step 1: load aux data of the current release
# Step 2: check for changes with an older version
# Step 3: write inventory, a dataframe
# Step 4: save inventory

update_aux_inventory <- function(measure      = "cpi",
                                 maindir      = getOption("pipaux.working_dir"),
                                 old_release  = NULL,
                                 key_cols.x   = NULL,
                                 key_cols.y   = NULL,
                                 ext          = c("qs", "csv"),
                                 verbose      = TRUE) {
  # _________________________#
  # Get arguments ####

  ext <- match.arg(ext)

  if (is.null(old_release)) {
    stop("Release to compare with must be provided, specifying date and identity.")
  }

  if (is.null(key_cols.x) || is.null(key_cols.y)) {

    ids_new <- joyn::possible_ids(new_df,
                                  verbose = verbose)
    ids_old <- joyn::possible_ids(old_df,
                                  verbose = verbose)

    key_cols.x <- ids_new[[1]]
    key_cols.y <- ids_old[[1]]
  }


  # Get current release ####

  pipfun::get_wrk_release()

  release <- paste0(wrk_release$release,
                    "_",
                    wrk_release$identity)

  # _________________________#
  # Load files with error handling

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

  if (is.null(old_df)) return(invisible(NULL))


  # _________________________#
  # Extract differences

  # Run comparison
  myr_obj <- myrror::myrror(
              dfx                 = new_df,
              dfy                 = old_df,
              by.x                = key_cols.x, # keys for matching (e.g., country and year)
              by.y                = key_cols.y, # keys for matching (e.g., country and year)
              compare_type        = FALSE,
              compare_values      = TRUE,
              extract_diff_values = TRUE,
              interactive         = FALSE)

  # diff_list <- myrror::extract_diff_values(myrror_object = myr_obj,
  #                                          output        = "simple")

  # Extract differences in table format
  diff_table <- myrror::extract_diff_table(myrror_object = myr_obj,
                                           by.x          = key_cols.x,
                                           by.y          = key_cols.y,
                                           output        = "simple",
                                           interactive   = FALSE)

  # If there are no differences, exit early
  if (nrow(diff_table) == 0) {

    cli::cli_alert_info(
      "No differences found for {.strong {measure}}. Inventory not updated.")
    return(invisible(NULL))

  }

  # Add metadata: files paths, measure

  new_path <- fs::path(maindir,
                       "aux_data",
                       release,
                       measure,
                       paste0(measure, ".", ext))

  old_path <- fs::path(maindir,
                       "aux_data",
                       old_release,
                       measure,
                       paste0(measure, ".", ext))

  diff_table <- diff_table |>
    fmutate(measure = measure,
            path.x  = new_path,
            path.y  = old_path)

  # _________________________#
  # Create or update inventory

  # Build inventory directory (path)
  inventory_dir <- fs::path(maindir,
                            "aux_data",
                            release,
                            "_inventory_aux_changes",
                            paste0("from_", old_release, "_to_", release))

  # Ensure the folder exists
  fs::dir_create(inventory_dir)  # this does nothing if folder already exists

  # Build file path: inventory_dir/measure.extension
  file_path <- fs::path(inventory_dir,
                        paste0(measure, ".", ext))

  # Save based on extension
  if (ext == "qs") {
    qs::qsave(diff_table, file_path)
  } else if (ext == "csv") {
    readr::write_csv(diff_table, file_path)
  }


  if (verbose) {cli::cli_alert_success(
    "Inventory file succesfully updated for measure: {.strong {measure}}")}

  invisible(TRUE)

}
