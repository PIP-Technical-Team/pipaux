# Auxiliary function that creates an inventory of value changes per country/year of the following measures:
# cpi, ppp, pfw, pop, gdp, pce

#' Compare auxiliary data files across releases
#'
#' Compares the contents of auxiliary data files between the current and a
#' specified previous release, identifying differences in values and rows for
#' a given measure.
#'
#' The release identifiers must follow the format `"YYYYMMDD_identity"`
#' (e.g., `"20250101_TEST"`).
#'
#' @inheritParams aux_fun
#' @param old_release Character. The identifier of the previous release to
#'   compare against (e.g., `"20240101_PROD"`). If `NULL`, automatically uses
#'   the last available release matching the same identity as the current
#'   working release.
#' @param verbose Logical. If `TRUE`, displays messages in the console.
#'   Default is `TRUE`.
#'
#' @return A named list with one element per measure. Each element is a list containing:
#'   \describe{
#'     \item{diff_values}{A data table of value-level differences across matched
#'       rows and columns. Columns with `.x` suffix refer to the current release
#'       (new data); `.y` suffix refers to the previous release (old data).
#'       `NULL` if no differences found.}
#'     \item{diff_rows}{A data table of rows added or removed between releases,
#'       with `change_type` column (`"added"` = in current release only,
#'       `"removed"` = in previous release only). `NULL` if no row differences found.}
#'   }
#'   
#'   Each measure's list also has attributes: `key_cols` (primary key columns used
#'   for comparison), `measure`, `new_path` (current release path), `old_path`
#'   (previous release path), `release`, and `old_release`.
#'   
#'   Returns `NULL` for a measure if data cannot be loaded or comparison fails.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' get_aux_changes(measure = "cpi", old_release = "20240101_PROD")
#' }
get_aux_changes <- function(measure = "cpi",
                            old_release = NULL,
                            verbose = TRUE) {

  stopifnot(is.character(measure), length(measure) == 1)

  wrk_release <- get_from_auxenv("wrk_release")
  aux_data_path <- get_from_auxenv("aux_data_path")
  release <- paste0(wrk_release$release, "_", wrk_release$identity)

  # Find old release if not provided
  if (is.null(old_release)) {
    old_release <- get_last_release(
      aux_data_path = aux_data_path,
      current_release = release,
      identity = wrk_release$identity
    )
    if (verbose) cli::cli_alert_info("Using last available release: {.strong {old_release}}")
  }

  # Load new data (current release)
  new_df <- tryCatch({
    pipload::load_aux_data(measure = measure)
  }, error = function(e) {
    cli::cli_alert_danger("Failed to load data for {.strong {measure}} in current release {.strong {release}}")
    stop(e)
  })

  # Load old data (previous release) using pip_read and explicit path
  release_root <- fs::path_dir(aux_data_path)
  id_path <- fs::path(release_root, old_release, paste0(measure, ".qs2"), measure, ext = "qs2")
  
  old_df <- tryCatch({
    qs2::qs_read(file = id_path)
    #pipload::pip_read(id = id_path, format = "qs2", verbose = verbose)
  }, error = function(e) {
    cli::cli_alert_warning(
      "Failed to load OLD data for {.strong {measure}} in release {.strong {old_release}}. Comparison will be skipped.")
    return(NULL)
  })

  if (is.null(old_df)) {
    cli::cli_alert_warning(
      "Empty OLD data for {.strong {measure}} in release {.strong {old_release}}. Comparison will be skipped.")
    return(invisible(NULL))
  }

  key_cols <- stamp::st_get_pk(new_df)
  
  if (is.null(key_cols) || !is.character(key_cols) || length(key_cols) == 0) {
    cli::cli_abort("Key variables could not be retrieved from data attributes.")
  }
  if (!all(key_cols %in% names(old_df))) {
    cli::cli_abort("Some key columns are missing in the old dataset: {setdiff(key_cols, names(old_df))}")
  }

  if (verbose) {
    cli::cli_alert_info("Keys used for comparison: {.var {key_cols}}")
  }

  # common_cols <- intersect(names(new_df), names(old_df))
  # new_df <- new_df[, common_cols, with = FALSE]
  # old_df <- old_df[, common_cols, with = FALSE]

  myr_obj <- tryCatch(
    myrror::myrror(
      dfx = new_df,
      dfy = old_df,
      by = key_cols,
      compare_type = FALSE,
      compare_values = TRUE,
      extract_diff_values = TRUE,
      interactive = FALSE,
      verbose = verbose
    ),
    error = function(e) {
      cli::cli_alert_danger(glue::glue(
        "myrror comparison failed for measure {.strong {measure}}: {e$message}"
      ))
      return(NULL)
    }
  )

  if (!is.null(myr_obj)) {
    diff_table <- myrror::extract_diff_table(myrror_object = myr_obj,
                                             by = key_cols,
                                             output = "simple",
                                             interactive = FALSE)
    diff_rows <- myrror::extract_diff_rows(myrror_object = myr_obj,
                                           by = key_cols,
                                           output = "simple",
                                           verbose = verbose)
  } else {
    diff_table <- NULL
    diff_rows <- NULL
  }

  # Add metadata as attributes
  if (!is.null(diff_rows)) {
    diff_rows[, change_type := fifelse(df == "dfx", "added", "removed")]
  }

  if (verbose) {
    cli::cli_alert_success("Diff values extracted successfully for measure: {.strong {measure}}")
  }

  result <- list(
    "diff_values" = diff_table,
    "diff_rows"   = diff_rows
  )
  setattr(result, "key_cols", key_cols)
  setattr(result, "measure", measure)
  setattr(result, "new_path", aux_data_path)
  setattr(result, "old_path", fs::path(aux_data_path, old_release))
  setattr(result, "release", release)
  setattr(result, "old_release", old_release)
  
  return(invisible(result))
}

#' Compare auxiliary data across measures between two releases
#'
#' Compares auxiliary data files between the current and a previous release
#' across one or more measures, by calling [get_aux_changes()] for each.
#'
#' @param measure Character vector of one or more measures, specifying measures to check
#'   (e.g., `c("cpi", "gdp")`). If `NULL`, all measures available in the
#'   GitHub organisation are included.
#' @param owner Character. GitHub owner of the auxiliary data repositories.
#'   Defaults to `"PIP-Technical-Team"`.
#' @inheritParams get_aux_changes
#' @param ... Additional arguments passed to [get_aux_changes()].
#'
#' @return A named list with one element per measure. Each element is a list containing:
#'   \describe{
#'     \item{diff_values}{A data table of value-level differences across matched
#'       rows and columns. Columns with `.x` suffix refer to the current release
#'       (new data); `.y` suffix refers to the previous release (old data).
#'       `NULL` if no differences found.}
#'     \item{diff_rows}{A data table of rows added or removed between releases,
#'       with `change_type` column (`"added"` = in current release only,
#'       `"removed"` = in previous release only). `NULL` if no row differences found.}
#'   }
#'   
#'   Each measure's list also has attributes: `key_cols` (primary key columns used
#'   for comparison), `measure`, `new_path` (current release path), `old_path`
#'   (previous release path), `release`, and `old_release`.
#'   
#'   Returns `NULL` for a measure if data cannot be loaded or comparison fails.
#' @export
#'
#' @examples
#' \dontrun{
#' compare_aux_releases(measure = c("cpi", "gdp"), old_release = "20240101_PROD")
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
#' Searches the `aux_data` parent directory for release folders and returns
#' the latest one prior to `current_release` that matches the specified
#' identity (e.g., `"PROD"` or `"TEST"`).
#'
#' @param aux_data_path Character. Path to the current release's `aux_data`
#'   directory. The function moves up one level to find sibling release folders.
#' @param current_release Character. Current release string in the format
#'   `"YYYYMMDD_identity"` (e.g., `"20260202_TEST"`).
#' @param identity Character. The identity suffix to filter releases
#'   (e.g., `"PROD"`, `"TEST"`). Case-sensitive.
#'
#' @return A character scalar with the most recent matching release prior to
#'   `current_release`. Throws an error if no older matching release is found.
#'
#' @keywords internal
get_last_release <- function(aux_data_path,
                             current_release,
                             identity) {
  # Move up one level if aux_data_path already includes the release
  release_root <- fs::path_dir(aux_data_path)

  # List all release folders under the root
  release_names <- fs::dir_ls(release_root,
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
#' Compares the most recent (latest) version of an auxiliary data file with an
#' earlier "vintage" version stored under the same release, identifying
#' differences in values and rows.
#'
#' @param measure Character. The name of the auxiliary measure to compare
#'   (e.g., `"gdp"`, `"pop"`).
#' @param verbose Logical. If `TRUE`, messages about the comparison process are
#'   printed. Default is `FALSE`.
#' @param version Integer. A negative integer indicating how many versions
#'   before the latest to compare with. For example, `-1` (default) compares
#'   with the version immediately prior; `-2` goes two versions back.
#'
#' @return Invisibly returns a named list with:
#'   \describe{
#'     \item{diff_values}{A data table of value-level differences. Columns with
#'       `.x` suffix refer to the latest version (new data); `.y` suffix refers
#'       to the earlier version (old data). `NULL` if no differences found.}
#'     \item{diff_rows}{A data table of rows added or removed between versions,
#'       with `change_type` column (`"added"` = in latest version only,
#'       `"removed"` = in earlier version only). `NULL` if no row differences found.}
#'   }
#'   The list has attributes: `key_cols` (primary key columns), `measure`,
#'   `new_path` (path to latest version), `old_path` (path to earlier version),
#'   `new_version_id`, `old_version_id`, and `release`.
#'   If no previous version is available, returns a list with all elements set
#'   to `NULL`.
#'
#' @seealso [compare_aux_vintages()], [pipload::load_aux_data()],
#'   [myrror::myrror()]
#' @export
#'
#' @examples
#' \dontrun{
#' compare_vintage_versions("cpi")
#' compare_vintage_versions("gdp", version = -2, verbose = TRUE)
#' }
compare_vintage_versions <- function(measure,
                                     verbose = FALSE,
                                     version = -1) {

  # Get release info early for consistent returns
  wrk_release <- get_from_auxenv("wrk_release")
  release <- paste0(wrk_release$release, "_", wrk_release$identity)

  # Load the most recent version
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
    result <- list(diff_values = NULL, diff_rows = NULL)
    key_cols <- stamp::st_get_pk(new_df)
    setattr(result, "key_cols", key_cols)
    setattr(result, "measure", measure)
    setattr(result, "new_path", NA_character_)
    setattr(result, "old_path", NA_character_)
    setattr(result, "new_version_id", NA_character_)
    setattr(result, "old_version_id", NA_character_)
    setattr(result, "release", release)
    return(invisible(result))
  }

  # Get version metadata using stamp::st_info
  st_info <- tryCatch({
    stamp::st_info(path = paste0(measure, ".qs2"), alias = "aux")
  }, error = function(e) {
    cli::cli_alert_danger("Failed to retrieve version metadata for {.strong {measure}}: {e$message}")
    NULL
  })

  if (is.null(st_info)) {
    result <- list(diff_values = NULL, diff_rows = NULL)
    key_cols <- stamp::st_get_pk(new_df)
    setattr(result, "key_cols", key_cols)
    setattr(result, "measure", measure)
    setattr(result, "new_path", NA_character_)
    setattr(result, "old_path", NA_character_)
    setattr(result, "new_version_id", NA_character_)
    setattr(result, "old_version_id", NA_character_)
    setattr(result, "release", release)
    return(invisible(result))
  }

  new_version_id <- st_info$catalog$latest_version_id
  new_path <- st_info$snapshot_dir
  
  # Get old version ID from version history
  if (!is.null(st_info$catalog$version_ids) && 
      length(st_info$catalog$version_ids) > abs(version)) {
    old_version_id <- st_info$catalog$version_ids[length(st_info$catalog$version_ids) - abs(version)]
    old_path <- fs::path(fs::path_dir(new_path), old_version_id)
  } else {
    cli::cli_alert_warning(
      "Cannot retrieve old version ID. Ensure enough versions exist."
    )
    old_version_id <- NA_character_
    old_path <- NA_character_
  }

  # Determine key columns
  key_cols <- stamp::st_get_pk(new_df)

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

  data.table::setorderv(new_df, cols = key_cols)
  data.table::setorderv(old_df, cols = key_cols)

  # Compare using myrror
  myr <- tryCatch({
    myrror::myrror(
      dfx                 = new_df,
      dfy                 = old_df,
      by                  = key_cols,
      compare_type        = FALSE,
      compare_values      = TRUE,
      extract_diff_values = TRUE,
      interactive         = FALSE,
      verbose             = verbose
    )
  }, error = function(e) {
    cli::cli_alert_danger(
      "myrror comparison failed for measure {.strong {measure}}: {e$message}"
    )
    NULL
  })

  if (is.null(myr)) {
    diff_vals <- NULL
    diff_rows <- NULL
  } else {
    diff_vals <- myrror::extract_diff_table(myrror_object = myr,
                                            by = key_cols,
                                            output = "simple",
                                            interactive = FALSE)
    diff_rows <- myrror::extract_diff_rows(myrror_object = myr,
                                           by = key_cols,
                                           output = "simple",
                                           verbose = verbose)
  }

  # Handle empty diff_rows
  if (is.null(diff_rows) || nrow(diff_rows) == 0) diff_rows <- NULL

  # Add change_type to diff_rows
  if (!is.null(diff_rows)) {
    diff_rows[, change_type := fifelse(df == "dfx", "added", "removed")]
  }

  # Report results
  if (!is.null(diff_vals) || !is.null(diff_rows)) {
    cli::cli_alert_success(
      "Vintage comparison complete for {.strong {measure}}. Differences detected."
    )
  } else {
    cli::cli_alert_success(
      "Vintage comparison complete for {.strong {measure}}. No differences found."
    )
  }

  # Return result with attributes
  result <- list(
    diff_values = diff_vals,
    diff_rows   = diff_rows
  )
  setattr(result, "key_cols", key_cols)
  setattr(result, "measure", measure)
  setattr(result, "new_path", new_path)
  setattr(result, "old_path", old_path)
  setattr(result, "new_version_id", new_version_id)
  setattr(result, "old_version_id", old_version_id)
  setattr(result, "release", release)

  return(invisible(result))
}
#' Compare vintage versions across multiple auxiliary data measures
#'
#' Applies [compare_vintage_versions()] across multiple auxiliary data measures.
#' Useful for tracking within-release changes across several files simultaneously.
#'
#' @param measures Character vector. Names of auxiliary data measures to compare
#'   (e.g., `c("gdp", "pop", "pfw")`). If `NULL` or empty, returns an empty
#'   list with a warning.
#' @param version Integer. Indicates how many versions before the latest to
#'   compare with (e.g., `-1` for the immediately previous version).
#'   Default is `-1`.
#' @param verbose Logical. If `TRUE`, messages about the comparison process are
#'   printed. Default is `FALSE`.
#'
#' @return Invisibly returns a named list with one element per measure,
#'   each being the output of [compare_vintage_versions()]. Elements are `NULL`
#'   for measures where no previous version exists or an error occurred.
#'
#' @seealso [compare_vintage_versions()]
#' @export
#'
#' @examples
#' \dontrun{
#' compare_aux_vintages(measures = c("cpi", "pop", "gdp"))
#' compare_aux_vintages(measures = c("cpi", "gdp"), version = -2, verbose = TRUE)
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
