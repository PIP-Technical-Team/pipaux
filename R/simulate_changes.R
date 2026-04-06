#' Simulate changes by modifying a measure's artifact and save it using pip_aux_save
#'
#' @param measure Character. The name of the measure artifact (e.g. "gdp", "ppp", "countries")
#' @param seed An optional seed for reproducibility
#' @param drop Numeric vector of row indices to drop (optional)
#' @param indices Numeric vector of row indices to modify for the measure variable (optional)
#' @param verbose Logical. Whether to print messages.
#'
#' @return Invisibly returns the modified data.table
#' @keywords internal
simulate_changes <- function(measure,
                                 seed = 123,
                                 drop = NULL,
                                 indices = NULL,
                                 verbose = TRUE) {
  
  # Load data from current working release
  dt <- pipload::load_aux_data(measure = measure, verbose = verbose)
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

  # Get primary key columns
  key_cols <- stamp::st_get_pk(dt)
  
  # Read sidecar to get gh attribute
  ext <- "qs2"
  sidecar_path <- fs::path(get_from_auxenv("aux_data_path"), measure, ext = ext)
  sidecar <- stamp::st_read_sidecar(sidecar_path)
  gh <- sidecar$gh
  
  # Get the function for code hash
  fun_name <- paste0("aux_", measure)
  aux_fun <- get(fun_name, mode = "function")

  # Save modified data using pip_aux_save
  pip_aux_save(
    x          = dt,
    id         = measure,
    pk         = key_cols,
    metadata   = list(gh = gh),
    code       = aux_fun,
    code_label = fun_name,
    format     = "qs2"
  )

  cli::cli_alert_success(
    "Modified and saved simulated version of '{measure}'"
  )

  invisible(dt)
}

