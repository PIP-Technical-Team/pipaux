#' Simulate an "old" version by modifying a measure's artifact
#'
#' @param old_release Character. The name of the old release to simulate (e.g. "20250101_TEST")
#' @param measure Character. The name of the measure artifact (e.g. "gdp", "ppp", "countries")
#' @param seed An optional seed for reproducibility
#'
#' @return Invisibly returns the modified data.table
#' @keywords internal
#' Simulate an "old" version by modifying a measure's artifact
#'
#' @param old_release Character. The name of the old release to simulate (e.g. "20250101_TEST")
#' @param measure Character. The name of the measure artifact (e.g. "gdp", "ppp", "countries")
#' @param seed An optional seed for reproducibility
#' @param drop Numeric vector of row indices to drop (optional)
#' @param indices Numeric vector of row indices to modify for the measure variable (optional)
#' @param verbose Logical. Whether to print messages.
#'
#' @return Invisibly returns a list with the modified data.table 
#' @keywords internal
simulate_old_release <- function(old_release = "20250101_TEST",
                                 measure,
                                 seed = 123,
                                 drop = NULL,
                                 indices = NULL,
                                 verbose = TRUE) {
  # Get aux_data_path for the old release by replacing the release folder in the current path
  current_path <- get_from_auxenv("aux_data_path")
  old_aux_data_path <- sub("[^/\\\\]+$", old_release, current_path)
  measure_dir <- file.path(old_aux_data_path, measure)
  if (is.null(measure_dir) || !dir.exists(measure_dir)) {
    dir.create(measure_dir, recursive = TRUE)
    if (verbose) cli::cli_alert_info("Created measure directory for '{measure}' in release '{old_release}': {measure_dir}")
  }

  # Always load data for the measure from the current working release
  dt <- pipload::load_aux_data(measure = measure)
  if (!data.table::is.data.table(dt)) dt <- data.table::as.data.table(dt)

  set.seed(seed)

  # Drop specified rows
  if (!is.null(drop) && length(drop) > 0) {
    n_before <- nrow(dt)
    dt <- dt[-drop, ]
    if (verbose) cli::cli_alert_info("Dropped {length(drop)} rows. Rows before: {n_before}, after: {nrow(dt)}.")
  }

  # Determine which column to modify
  target_col <- if (measure == "cpi") {
    "cpi_value"
  } else if (measure == "pfw") {
    "survey_comparability"
  } else {
    measure
  }

  # Modify target column at given indices
  if (!is.null(indices) && length(indices) > 0 && target_col %in% names(dt)) {
    valid_indices <- indices[indices %in% seq_len(nrow(dt))]
    # Choose replacement values depending on measure
    if (measure == "pfw") {
      replacement_values <- rep(1, length(valid_indices))
    } else {
      replacement_values <- rep(c(0.100, 0.150, 0.200), length.out = length(valid_indices))
    }
    dt[valid_indices, (target_col) := replacement_values]
    if (verbose) {
      if (measure == "pfw") {
        cli::cli_alert_info(
          "Modified '{target_col}' at {length(valid_indices)} rows with value 1."
        )
      } else {
        cli::cli_alert_info(
          "Modified '{target_col}' at {length(valid_indices)} rows with fixed values 0.100, 0.150, 0.200."
        )
      }
    }
  }

  # Save modified data to old release aux_data_path
  # make sure to init stamp
  #stamp::st_init(root = , alias = "test_simulate_old_release")
  pipload::pip_write(
    x = dt,
    id = measure_dir,
    #dir = measure_dir,
    format = "qs2"
  )

  cli::cli_alert_success(
    "Modified and saved simulated old version of '{measure}' in aux_data_path for release: {old_release}"
  )

  invisible(list(data = dt, aux_data_path = old_aux_data_path))
}

#' Simulate file changes for a measure
#'
#' Loads, modifies, and saves auxiliary data for a given measure, following the standard aux_ workflow.
#'
#' @param measure Character. Name of the auxiliary data measure (e.g., "cpi", "ppp").
#' @param seed Optional integer. Random seed for reproducibility.
#' @param ... Additional arguments passed to pip_aux_save.
#' @return Invisibly returns the modified data.table.
#' @export
simulate_file_changes <- function(measure, seed = 123, ...) {
  dt <- pipload::load_aux_data(measure = measure)
  if (!data.table::is.data.table(dt)) dt <- data.table::as.data.table(dt)
  set.seed(seed)

  n_rows <- nrow(dt)
  if (n_rows < 1) return(invisible(dt))
  n_mod <- min(3, n_rows)
  idx <- sample(seq_len(n_rows), n_mod)

  # Modify 'year' column if present
  if ("year" %in% names(dt)) {
    dt[idx, year := year + sample(c(-1, 1), n_mod, replace = TRUE)]
  }

  # Modify column named by 'measure' if present and numeric
  if (measure %in% names(dt) && is.numeric(dt[[measure]])) {
    dt[idx, (measure) := get(measure) * runif(n_mod, 0.9, 1.1)]
  }

  dt[, simulated_flag := TRUE]
  pip_aux_save(x = dt, id = measure, ...)
  invisible(dt)
}
