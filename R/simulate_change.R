#' Simulate changes in a vintage data file
#'
#' Introduces small random changes to an existing file to simulate an updated or modified version.
#' Overwrites the original file with the modified data.
#'
#' @param file_path Character. Full path to the file to be modified (supported: .qs, .rds, .csv).
#' @param seed Optional integer. Random seed for reproducibility.
#'
#' @return Invisibly returns the modified `data.table`. The original file is overwritten.
#' @keywords internal
simulate_file_changes <- function(file_path,
                                  seed = 123
                                  ) {

  stopifnot(file.exists(file_path))

  # Load data depending on extension
  ext <- tools::file_ext(file_path)

  dt <- switch(ext,
               qs  = qs::qread(file_path),
               rds = readRDS(file_path),
               csv = data.table::fread(file_path),
               stop("Unsupported file extension: ", ext))

  if (!data.table::is.data.table(dt))

    dt <- data.table::as.data.table(dt)

  if (nrow(dt) < 2) {

    cli::cli_alert_warning("Data has fewer than 2 rows, skipping changes.")
    return(invisible(dt))

  }

  set.seed(seed)

  # --- 1. Modify "year" column if it exists ---
  if ("year" %in% names(dt)) {

    idx <- sample(seq_len(nrow(dt)),
                  min(3,
                      nrow(dt)))

    dt[idx, year := year + sample(c(-1, 1),
                                  length(idx),
                                  replace = TRUE)]
    message("Modified 'year' in ",
            length(idx),
            " rows.")
  }

  # --- 2. Modify a numeric column ---
  num_cols <- names(dt)[sapply(dt, is.numeric)]

  if (length(num_cols) > 0) {

    col_to_change <- sample(num_cols, 1)

    idx <- sample(seq_len(nrow(dt)),
                  min(3, nrow(dt)))

    dt[idx, (col_to_change) := get(col_to_change) * runif(length(idx),
                                                          0.9, 1.1)]
    message("Modified '",
            col_to_change, "' in ",
            length(idx), " rows.")

  }

  # --- 3. Structural changes (optional) ---
  dt <- dt[-.N]                      # Remove last row
  dt[, simulated_flag := TRUE]      # Add dummy column

  # --- 4. Save the modified version ---


  switch(ext,
         qs  = qs::qsave(dt,
                         file_path),
         rds = saveRDS(dt,
                       file_path),
         csv = data.table::fwrite(dt,
                                  file_path))

  cli::cli_alert_success("Saved modified file as: {file_path}")
  invisible(dt)
}

#' Simulate an "old" version by modifying a measure's pin
#'
#' @param old_release Character. The name of the old release board to simulate (e.g. "20250101_TEST")
#' @param measure Character. The name of the measure pin (e.g. "gdp", "ppp", "countries")
#' @param seed An optional seed for reproducibility
#'
#' @return Invisibly returns the modified data.table
#' @keywords internal
#' Simulate an "old" version by modifying a measure's pin
#'
#' @param old_release Character. The name of the old release board to simulate (e.g. "20250101_TEST")
#' @param measure Character. The name of the measure pin (e.g. "gdp", "ppp", "countries")
#' @param seed An optional seed for reproducibility
#' @param drop Numeric vector of row indices to drop (optional)
#' @param indices Numeric vector of row indices to modify for the measure variable (optional)
#' @param verbose Logical. Whether to print messages.
#'
#' @return Invisibly returns a list with the modified data.table and board
#' @keywords internal
#' Simulate an "old" version by modifying a measure's pin
#'
#' @param old_release Character. The name of the old release board to simulate (e.g. "20250101_TEST")
#' @param measure Character. The name of the measure pin (e.g. "gdp", "ppp", "countries")
#' @param seed An optional seed for reproducibility
#' @param drop Numeric vector of row indices to drop (optional)
#' @param indices Numeric vector of row indices to modify (optional)
#' @param verbose Logical. Whether to print messages.
#'
#' @return Invisibly returns a list with the modified data.table and board
#' @keywords internal
simulate_old_release <- function(old_release = "20250101_TEST",
                                 measure,
                                 seed = 123,
                                 drop = NULL,
                                 indices = NULL,
                                 verbose = TRUE) {

  # --- Get the current board from aux environment ---
  board_current <- get_from_auxenv("aux_data_board")
  if (is.null(board_current)) {
    cli::cli_abort("Current aux_data_board not found in aux environment.")
  }

  # --- Retrieve or create the board for the old release ---
  board_old <- get_aux_board(release = old_release, verbose = verbose)

  # --- Load data from current board ---
  if (!pins::pin_exists(board_current, measure)) {
    cli::cli_abort("Measure '{measure}' does not exist in the current release board.")
  }

  dt <- pipload::pip_read(board = board_current,
                          pin_name = measure,
                          verbose = FALSE)

  if (!data.table::is.data.table(dt)) dt <- data.table::as.data.table(dt)

  set.seed(seed)

  # --- Drop specified rows ---
  if (!is.null(drop) && length(drop) > 0) {
    n_before <- nrow(dt)
    dt <- dt[-drop, ]
    if (verbose) cli::cli_alert_info("Dropped {length(drop)} rows. Rows before: {n_before}, after: {nrow(dt)}.")
  }

  # --- Determine which column to modify ---
  target_col <- if (measure == "cpi") "cpi_value" else measure

  # --- Modify target column at given indices ---
  if (!is.null(indices) && length(indices) > 0 && target_col %in% names(dt)) {
    valid_indices <- indices[indices %in% seq_len(nrow(dt))]
    replacement_values <- rep(c(0.100, 0.150, 0.200), length.out = length(valid_indices))
    dt[valid_indices, (target_col) := replacement_values]
    if (verbose) cli::cli_alert_info(
      "Modified '{target_col}' at {length(valid_indices)} rows with fixed values 0.100, 0.150, 0.200."
    )
  }

  # --- Save modified data to old release board ---
  pipload::pip_write(
    board = board_old,
    x = dt,
    pin_name = measure,
    force_identical_write = TRUE
  )

  cli::cli_alert_success(
    "Modified and saved simulated old version of '{measure}' in old release board: {old_release}"
  )

  invisible(list(data = dt, board = board_old))
}
