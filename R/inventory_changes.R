# Auxiliary function that creates an inventory of value changes per country/year of the following measures:
# cpi, ppp, pfw, pop, gdp

#' Compare auxiliary data files across releases
#'
#' This function compares the contents of auxiliary data files between the current and a specified previous release.
#' It identifies differences in values for specified key variables
#'
#' The release identifiers must follow the format `"YYYYMMDD_identity"` (e.g., `"20250101_TEST"`).
#'
#' @inheritParams aux_fun
#' @param old_release Character. The identifier of the previous release to compare against (e.g., `"20240101_PROD"`).
#' @param key_cols Character vector. Variables used as keys to compare values between releases. Defaults to the `pipaux.key_vars` option.
#' @param verbose Logical. If `TRUE`, displays messages in the console.
#'
#' @return A data frame with detected differences in values, grouped by the specified key variables.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' get_aux_changes(measure = "cpi", old_release = "20240101_PROD")
#' }
get_aux_changes <- function(measure      = "cpi",
                            maindir      = getOption("pipaux.working_dir"),
                            old_release  = NULL,
                            key_cols     = getOption("pipaux.key_vars"),
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

  # new_path <- fs::path(maindir,
  #                      "aux_data",
  #                      release,
  #                      measure,
  #                      paste0(measure, ".", "qs"))
  #
  # old_path <- fs::path(maindir,
  #                      "aux_data",
  #                      old_release,
  #                      measure,
  #                      paste0(measure, ".", "qs"))
  #
  # diff_table <- diff_table |>
  #   fmutate(measure = measure,
  #           path.x  = new_path,
  #           path.y  = old_path)

  # _______________________________________#
  # Return ####

  if (verbose) {
    cli::cli_alert_success("Diff values extracted successfully for measure: {.strong {measure}}")
  }

  return(diff_table)

}

#' Inventory of changes in auxiliary data across measures
#'
#' Compares auxiliary data files between the current and a specified previous release across one or more measures.
#'
#' @param measure Optional character vector. Specific measures to check (e.g., `c("cpi", "gdp")`). If `NULL`, all available measures are included.
#' @param maindir Path to the local auxiliary data directory. Defaults to `getOption("pipaux.working_dir")`.
#' @param owner GitHub owner of aux data repos. Defaults to `"PIP-Technical-Team"`.
#' @inheritParams get_aux_changes
#' @param ... Additional arguments passed to `get_aux_changes()`.
#'
#' @return Named list of data frames with value differences for each measure. Measures with no differences or errors return `NULL`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' inventory_aux_changes(old_release = "20240101_PROD", verbose = TRUE)
#' }
inventory_aux_changes <- function(measure     = NULL,
                                  maindir     = getOption("pipaux.working_dir"),
                                  owner       = "PIP-Technical-Team",
                                  old_release = NULL,
                                  verbose     = FALSE,
                                  key_cols    = getOption("pipaux.key_vars"),
                                  ...) {

  # _______________________________________#
  # Get arguments ####

  # Get all measures from repos in PIP-Technical-Team that start with "aux_"

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


  # Return named list of changes for every measure

  return(res)

}
