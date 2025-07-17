#' Simulate an "old" version by modifying a measure's data file
#'
#' @param base_dir Base directory containing the version folders
#' @param root_dir Root directory of PIP data. Defaults tpo `Sys.getenv("PIP_ROOT_DIR")`
#' @param old_release The name of the old release folder to simulate changes in
#' @param measure Character. The name of the measure folder (e.g. "gdp")
#' @param seed An optional seed for reproducibility
#'
#' @return Invisibly returns the modified data.table
#' @keywords internal
simulate_old_release <- function(base_dir        = getOption("pipaux.working_dir"),
                                 root_dir        = Sys.getenv("PIP_ROOT_DIR"),
                                 old_release     = "20250101_TEST",
                                 measure,
                                 seed            = 123) {

  pipfun::get_wrk_release(verbose = FALSE)
  release  <- wrk_release$release
  identity <- wrk_release$identity

  current_version <- paste0(release, "_", identity)

  stopifnot(dir.exists(base_dir))

  version_path <- file.path(root_dir, base_dir, old_release)
  measure_path <- file.path(version_path, measure)
  file_path <- file.path(measure_path, paste0(measure, ".qs"))


  # Read data
  dt <- load_aux(measure = measure,
                 maindir = base_dir,
                 branch  = current_version)
  set.seed(seed)

  if (nrow(dt) < 2) {
    warning("Data has fewer than 2 rows, skipping changes.")
    return(invisible(dt))
  }

  if (measure == "ppp") {

    skip


  } else {

    # 1. Modify some year values (if column exists)
    if ("year" %in% names(dt)) {
      idx <- sample(seq_len(nrow(dt)), min(3, nrow(dt)))  # up to 3 changes
      dt[idx, year := year + sample(c(-1, 1), length(idx), replace = TRUE)]
      message("Modified 'year' column in ", length(idx), " rows.")
    } else {
      message("No 'year' column found.")
    }

    # 2. Modify some values of a numeric column matching the measure name
    num_cols <- names(dt)[sapply(dt, is.numeric)]
    match_cols <- grep(tolower(measure), tolower(num_cols), value = TRUE)

    if (length(match_cols) > 0) {
      col_to_change <- match_cols[1]
      idx <- sample(seq_len(nrow(dt)), min(3, nrow(dt)))
      dt[idx, (col_to_change) := get(col_to_change) * runif(length(idx), 0.9, 1.1)]
      message("Modified column: ", col_to_change, " in ", length(idx), " rows.")
    } else {
      warning("No numeric column matched the measure name: ", measure)
    }

    # 3. Optional structural differences
    dt <- dt[-.N]  # Remove last row
    dt[, mock_col := "simulated"]  # Add a dummy column

  }


  # SAve modified data
  pipfun::pip_sign_save(x       = dt,
                        measure = measure,
                        msrdir  = fs::path(base_dir, "aux_data", old_release, measure))

  message("Modified and saved simulated old version at: ", file_path)

  invisible(dt)
}


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




