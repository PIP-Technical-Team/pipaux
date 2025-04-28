# Auxiliary function that creates an inventory of value changes per country/year of the following measures:
# cpi, ppp, pfw, pop, gdp

# Step 1: load aux data of the current release
# Step 2: check for changes with an older version
# Step 3: write inventory, a dataframe
# Step 4: save inventory

update_aux_inventory <- function(measure      = "cpi",
                                 maindir      = getOption("pipaux.working_dir"),
                                 old_release,
                                 #by_vars = c("country_code", "year"),
                                 ext = c("qs", "csv")) {

  # _________________________#
  # Get current release ####

  pipfun::get_wrk_release()

  release <- paste0(wrk_release$release,
                    "_",
                    wrk_release$identity)

  # _________________________#
  # Load files

  new_df <- load_aux(measure = measure,
                     maindir = maindir,
                     branch  = release)


  old_df <- load_aux(measure = measure,
                     maindir = maindir,
                     branch  = old_release)

  # _________________________#
  # Extract differences

  key_cols <- joyn::possible_ids(new_df)[1][[1]]

  # Run comparison
  myr_obj <- myrror::myrror(
              dfx                 = new_df,
              dfy                 = old_df,
              by                  = key_cols, # keys for matching (e.g., country and year)
              compare_type        = FALSE,
              compare_values      = TRUE,
              extract_diff_values = TRUE,
              interactive         = FALSE)

  diff_df <- myrror::extract_diff_values(myrror_object = myr_obj,
                                         output        = "simple")[[measure]]

  # Add info on: files paths, name of measure
  # Add measure name and path

  new_path <- fs::path(maindir,
                       "aux_data",
                       release,
                       measure,
                       measure,
                       ".qs")

  old_path <- fs::path(maindir,
                       "aux_data",
                       old_release,
                       measure,
                       measure,
                       ".qs")

  diff_df |>
    fmutate(measure = measure,
            path.x  = new_path,
            path.y  = old_path)

  # If there are no differences, exit early
  if (nrow(diff_df) == 0) {
    cli::cli_alert_info("No differences found. Inventory not updated.")
    return(invisible(NULL))
  }

  # _________________________#
  # Create or update inventory

  # Build inventory path and ensure directory
  inventory_path <- fs::path(maindir,
                             "aux_data",
                             "_inventory_aux_changes",
                             paste0(release, "_", old_release),
                             paste0(measure, ".", ext)) # to fix

  fs::dir_create(fs::path_dir(inventory_path))

  # Save
  switch(extension,
         qs  = qs::qsave(diff_table, inventory_path),
         csv = readr::write_csv(diff_table, inventory_path),
         stop("Extension must be 'qs' or 'csv'")
  )



}
