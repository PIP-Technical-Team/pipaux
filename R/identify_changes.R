# Auxiliary function that creates an inventory of value changes per country/year of the following measures:
# cpi, ppp, pfw, pop, gdp

#' Compare auxiliary data files across releases
#'
#' This function compares the contents of auxiliary data files between the current and a specified previous release.
#' It identifies differences for specified key variables
#'
#' The release identifiers must follow the format `"YYYYMMDD_identity"` (e.g., `"20250101_TEST"`).
#'
#' @inheritParams aux_fun
#' @param old_release Character. The identifier of the previous release to compare against (e.g., `"20240101_PROD"`).
#'        If NULL, automatically take last available release of same identity as the current working release
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
                            old_release  = NULL,
                            verbose      = TRUE) {

  # _______________________________________#
  # Get arguments ####

  stopifnot(is.character(measure),
            length(measure) == 1)


  # Current release ####
  wrk_release <- get_from_auxenv(key = "wrk_release")

  release     <- paste0(wrk_release$release,
                    "_",
                    wrk_release$identity)

  # Relase board ####
  ab <- get_from_auxenv("aux_data_board")

  # Old release ####
  if (is.null(old_release)) {

    old_release <- get_last_release(board           = ab,
                                    current_release = release,
                                    identity        = wrk_release$identity)

   if (verbose) cli::cli_alert_info("Using last available release: {.strong {old_release}}")
  }

  # Old release board
  ab_old <- get_aux_board(release = old_release,
                          verbose = FALSE)


  # _______________________________________#
  # Load files  ####

  new_df <- tryCatch({

    pipload::load_aux_data(measure = measure) # by default reads the latest available version in current release

  },

  error = function(e) {
    cli::cli_alert_danger("Failed to load data for {.strong {measure}} in current release {.strong {release}}")
    stop(e)
  })

  old_df <- tryCatch({

    pipload::pip_read(board    = ab_old,
                      pin_name = measure,
                      verbose  = FALSE)
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


  # Key vars to compare by ______ ####

  key_cols <- attributes(new_df)$aux_key

  if (is.null(key_cols) || !is.character(key_cols) || length(key_cols) == 0) {
    cli::cli_abort("Key variables could not be retrieved from data attributes.")
  }


  if (!all(key_cols %in% names(old_df))) {
    cli::cli_abort("Some key columns are missing in the old dataset: {setdiff(key_cols, names(old_df))}")
  }


  # _______________________________________#

  # Sort both datasets by key columns

  # setorderv(new_df,
  #           cols = key_cols)
  # setorderv(old_df,
  #           cols = key_cols)

  if (verbose) {
    cli::cli_alert_info("Keys used for comparison: {.var {key_cols}}")
  }

  # Extract differences __________ ####

  ## Run comparison

  # Find common columns
  common_cols <- intersect(names(new_df),
                           names(old_df))

  # Subset and order columns identically
  new_df <- new_df[, common_cols, with = FALSE]
  old_df <- old_df[, common_cols, with = FALSE]

  myr_obj <- tryCatch(
    myrror::myrror(
      dfx                 = new_df,
      dfy                 = old_df,
      by                  = key_cols, # keys for matching (e.g., country and year)
      compare_type        = FALSE,
      compare_values      = TRUE,
      extract_diff_values = TRUE,
      interactive         = FALSE,
      verbose             = verbose
    ),
    error = function(e) {
      cli::cli_alert_danger(glue::glue(
        "myrror comparison failed for measure {.strong {measure}}: {e$message}"
      ))
      return(NULL)
    }
  )

  ## Extract different values and different rows

  if (!is.null(myr_obj)) {
    diff_table <- myrror::extract_diff_table(myrror_object = myr_obj,
                                             by            = key_cols,
                                             output        = "simple",
                                             interactive   = FALSE)

    # Extract different rows
    diff_rows <- myrror::extract_diff_rows(myrror_object  = myr_obj,
                                           by             = key_cols,
                                           output         = "simple",
                                           verbose        = verbose)

  }

  else {
    diff_table <- NULL
    diff_rows <- NULL
  }


  # Add metadata: files paths, measure

  if (!is.null(diff_table)) {


    diff_table <- diff_table |>
      fmutate(measure = measure,
              new_path.x  = ab$path,
              old_path.y  = ab_old$path)

  }

  if (!is.null(diff_rows)) {
    diff_rows[, change_type := fifelse(df == "dfx",
                                       "added",
                                       "removed")]
    diff_rows[, `:=`(
      measure     = measure,
      release     = release,
      old_release = old_release
    )]

  }


  # _______________________________________#
  # Return ####

  if (verbose) {
    cli::cli_alert_success("Diff values extracted successfully for measure: {.strong {measure}}")
  }

  result <- list(
    "diff_values" = diff_table,
    "diff_rows"   = diff_rows
   # "diff_cols"   = diff_cols
  )

  setattr(result,
          "key_cols",
          key_cols)

  return(invisible(result))

}

#' Inventory of changes in auxiliary data across measures
#'
#' Compares auxiliary data files between the current and a previous release across one or more measures.
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
compare_aux_releases <- function(measure     = NULL,
                                 owner       = "PIP-Technical-Team",
                                 old_release = NULL,
                                 verbose     = FALSE,
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
        old_release = old_release,
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

#' Get the most recent previous release of a specific identity
#'
#' This function searches the `aux_data` directory inside the given main directory
#' and returns the latest available release (prior to a given current release)
#' that matches the specified identity (e.g., `"prod"` or `"dev"`).
#'
#' @param maindir Character. The root directory where `aux_data` subfolder is located.
#' @param current_release Character. Current release string in the format `"YYYYMMDD_identity"`.
#' @param identity Character. The identity suffix to filter releases (e.g., `"prod"`).
#'
#' @return A character string representing the most recent previous release.
#'
#' @keywords internal
get_last_release <- function(board,
                             current_release,
                             identity) {

  # Go up from current release folder to aux_data
  aux_path <- fs::path_dir(board$path)

  # List all release folders under aux_data
  release_names <- fs::dir_ls(aux_path,
                              type = "directory",
                              recurse = FALSE) |>
    fs::path_file()

  # Keep only folders matching pattern YYYYMMDD_identity
  valid_releases <- release_names[
    grepl(paste0("^\\d{8}_", identity, "$"), release_names)
  ]

  # Sort and pick the one just before current_release
  candidates <- sort(valid_releases[valid_releases < current_release],
                     decreasing = TRUE)

  if (length(candidates) == 0) {
    cli::cli_abort("No older release found with identity {.strong {identity}} prior to {.strong {current_release}}.")
  }

  return(candidates[1])
}



#' Compare two vintage versions of an auxiliary data file
#'
#' Compares the most recent version of an auxiliary data file with an earlier "vintage" version,
#' identifying differences in values and rows.
#'
#' @param measure Character. The name of the auxiliary measure to compare (e.g., "gdp", "pop").
#' @param verbose Logical. If `TRUE`, messages about the comparison process are printed. Default is `FALSE`.
#' @param version Integer. A negative number indicating how many versions before the latest one to compare with.
#'   For example, `-1` compares the current version with the one just before it, `-2` goes two versions back, etc.
#'
#' @return Invisibly returns a list with the following elements (if differences are found):
#' \describe{
#'   \item{diff_values}{A data table showing differences in values across matched rows and columns.}
#'   \item{diff_rows}{A data table showing rows added or removed between versions.}
#' }
#' If no previous version is available or no differences are found, returns `NULL`.
#'
#' @seealso [pipload::load_aux_data()], [myrror::myrror()]
#' @export
compare_vintage_versions <- function(measure,
                                     verbose = FALSE,
                                     version = -1) {

  # ------------------------------------------------------------#
  # Load the most recent version
  # ------------------------------------------------------------#
  new_df <- tryCatch({
    pipload::load_aux_data(measure = measure)
  },

  error = function(e) {
    cli::cli_alert_danger("Failed to load latest version of {.strong {measure}}.")
    stop(e)
  })

  # Load previous version
  old_df <- tryCatch({

    pipload::load_aux_data(measure = measure,
                                 version = version)

  },

  error = function(e) {
    cli::cli_alert_warning(
      "Failed to load previous version of {.strong {measure}}. Not enough versions?"
    )
    NULL
  })

  if (is.null(old_df)) {

    cli::cli_alert_warning(
      "Previous version of {.strong {measure}} is NULL. Comparison skipped."
    )

    return(invisible(list(diff_values = NULL,
                          diff_rows   = NULL,
                          key_cols    = NULL)))
  }

  # ------------------------------------------------------------#
  # Determine key columns
  # ------------------------------------------------------------#
  key_cols <- attributes(new_df)$aux_key

  if (length(key_cols) == 0) {
    cli::cli_abort("No key columns found in data attributes.")
  }

  # Ensure key columns exist in both datasets
  missing_keys <- setdiff(key_cols, names(old_df))

  if (length(missing_keys) > 0) {
    cli::cli_abort(
      "Old version of {.strong {measure}} is missing key columns: {paste(missing_keys, collapse=', ')}"
    )
  }

  data.table::setorderv(new_df,
                        cols = key_cols)
  data.table::setorderv(old_df,
                        cols = key_cols)

  # ------------------------------------------------------------#
  # Compare using myrror
  # ------------------------------------------------------------#
  myr <- myrror::myrror(
    dfx                 = new_df,
    dfy                 = old_df,
    by                  = key_cols,
    compare_type        = FALSE,
    compare_values      = TRUE,
    extract_diff_values = TRUE,
    interactive         = FALSE,
    verbose             = verbose
  )

  diff_vals <- myrror::extract_diff_table(myrror_object = myr,
                                          by = key_cols,
                                          output = "simple")
  diff_rows <- myrror::extract_diff_rows(myrror_object = myr,
                                         by = key_cols,
                                         output = "simple")

  # Handle empty diff_rows
  if (is.null(diff_rows) || nrow(diff_rows) == 0) diff_rows <- NULL

  # ------------------------------------------------------------#
  # TEMPORARY measure-specific renaming (CPI / PPP)
  # ------------------------------------------------------------#

  ## CPI ##
  if (measure == "cpi") {
    # diff values
    if (!is.null(diff_vals)) {
      diff_vals <- diff_vals |>
        frename(reporting_level = year.new,
                year = survey_acronym.new,
                survey_acronym = reporting_level.new)
      diff_vals <- diff_vals |>
        frename(year.new            = year,
                survey_acronym.new  = survey_acronym,
                reporting_level.new = reporting_level)
    }

    # diff rows
    if (!is.null(diff_rows)) {
      diff_rows <- diff_rows |>
        frename(reporting_level = year.new,
                year = survey_acronym.new,
                survey_acronym = reporting_level.new)
      diff_rows <- diff_rows |>
        frename(year.new = year,
                survey_acronym.new = survey_acronym,
                reporting_level.new = reporting_level)
    }
  }

  ## PPP ##
  if (measure == "ppp") {
    # diff values
    if (!is.null(diff_vals)) {
      diff_vals <- diff_vals |>
        frename(reporting_level = ppp_year.new,
                ppp_year = reporting_level.new)
      diff_vals <- diff_vals |>
        frename(ppp_year.new            = ppp_year,
                reporting_level.new = reporting_level)
    }

    # diff rows
    if (!is.null(diff_rows)) {
      diff_rows <- diff_rows |>
        frename(reporting_level = ppp_year.new,
                ppp_year = reporting_level.new)
      diff_rows <- diff_rows |>
        frename(ppp_year.new = ppp_year,
                reporting_level.new = reporting_level)
    }
  }

  # ------------------------------------------------------------#
  # Report results
  # ------------------------------------------------------------#
  if (!is.null(diff_vals) || !is.null(diff_rows)) {
    cli::cli_alert_success(
      "Vintage comparison complete for {.strong {measure}}. Differences detected."
    )
  }

  else {
    cli::cli_alert_success(
      "Vintage comparison complete for {.strong {measure}}. No differences found."
    )
  }

  # ------------------------------------------------------------#
  # Return result (always a list)
  # ------------------------------------------------------------#
  result <- list(
    diff_values = diff_vals,
    diff_rows   = diff_rows,
    key_cols    = key_cols
  )

  return(invisible(result))
}


#' Compare Vintage Versions for Multiple Auxiliary Data Files
#'
#' Applies [compare_vintage_versions()] across multiple auxiliary data measures.
#' Useful for tracking within-release changes across several files.
#'
#' @param measures Character vector. Names of auxiliary data measures to compare
#'   (e.g., `c("gdp", "pop", "pfw")`).
#' @param version Integer. Indicates how many versions before the latest to compare with
#'   (e.g., `-1` for previous).
#' @param verbose Logical. If `TRUE`, messages about the comparison process are printed.
#' @param ... Additional arguments passed to [compare_vintage_versions()].
#'
#' @return Invisibly returns a named list of results from [compare_vintage_versions()],
#'   one element per measure.
#'
#' @export
#' @examples
#' \dontrun{
#' compare_aux_vintages(measures = c("cpi", "pop", "gdp"))
#' }
compare_aux_vintages <- function(measures = NULL,
                                 version = -1,
                                 verbose = FALSE) {

  if (is.null(measures) || length(measures) == 0) {
    cli::cli_alert_warning("No measures provided. Nothing to compare.")
    return(invisible(list()))
  }

  results <- lapply(measures, function(m) {

    tryCatch({
      res <- compare_vintage_versions(
        measure = m,
        version = version,
        verbose = verbose
      )

      if (is.null(res)) {
        if (verbose) {
          cli::cli_alert_info("No previous version found for measure {.strong {m}}. Skipping.")
        }
        return(NULL)
      }

      return(res)
    }, error = function(e) {
      if (verbose) {
        cli::cli_alert_danger("Error comparing measure {.strong {m}}: {e$message}")
      }
      return(NULL)
    })
  })

  names(results) <- measures

  return(invisible(results))
}
