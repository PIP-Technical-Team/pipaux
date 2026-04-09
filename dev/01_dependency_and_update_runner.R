# =====================================================================
# DEPENDENCY + UPDATE ENGINE DIAGNOSTICS
# =====================================================================
# Purpose:
#   - Run update_aux_measures() safely
#   - Respect dependency ordering
#   - Extract structured diagnostics from log
#   - Summarize timing + status
#
# Usage:
#   source("dev/01_dependency_and_update_runner.R")
#   run_ordered_update_diagnostics()
#
#   # Optional subset
#   run_ordered_update_diagnostics(measures = c("cpi", "gdp"))
# =====================================================================

if (!interactive()) {
  stop("This script is for interactive development only.")
}

run_ordered_update_diagnostics <- function(
  measures = NULL,
  owner = getOption("pipfun.ghowner"),
  tag = NULL,
  log_save = TRUE,
  verbose = TRUE
) {

  cat("\n", strrep("=", 70), "\n")
  cat("AUX DATA UPDATE ENGINE DIAGNOSTICS\n")
  cat(strrep("=", 70), "\n\n")

  if (is.null(owner)) {
    stop("Owner must be set via option 'pipfun.ghowner' or provided.")
  }

  # ------------------------------------------------------------
  # 1. Run update engine
  # ------------------------------------------------------------

  start_total <- Sys.time()

  log_name <- init_aux_log(overwrite = TRUE)

  update_aux_measures(
    measures = measures,
    owner = owner,
    log = TRUE,
    log_overwrite = TRUE,
    verbose = verbose,
    halt_on_dep_fail = FALSE,
    log_save = TRUE,
    log_name = log_name
  )

  total_time <- as.numeric(Sys.time() - start_total)

  # ------------------------------------------------------------
  # 2. Retrieve log
  # ------------------------------------------------------------

  log_obj <- pipfun::log_load(id = "pipaux_update_log", alias = "aux_meta", overwrite = TRUE)

  if (is.null(log_obj)) {
    stop("No log found after update execution.")
  }

  if (!"logmeta" %in% names(log_obj)) {
    stop("Log object missing 'logmeta' column.")
  }

  # ------------------------------------------------------------
  # 3. Extract structured execution info
  # ------------------------------------------------------------

  log_dt <- data.table::as.data.table(log_obj)

  # Extract measure + step from logmeta list column
  log_dt[, measure := sapply(logmeta, function(x) x[["measure"]])]
  log_dt[, step    := sapply(logmeta, function(x) x[["step"]])]

  # Keep only rows tied to a specific measure
  update_steps <- log_dt[!is.na(measure) & measure != "NULL"]

  # One row per measure: count errors, derive status
  summary_dt <- update_steps[, .(
    n_log_entries = .N,
    n_errors      = sum(event == "error", na.rm = TRUE)
  ), by = measure]

  summary_dt[, status := data.table::fifelse(n_errors > 0, "error", "success")]

  # ------------------------------------------------------------
  # 4. Print clean summary
  # ------------------------------------------------------------

  cat("\nExecution Summary:\n")
  cat(strrep("-", 70), "\n")

  cat(sprintf("  %-20s  %-10s  %s\n", "measure", "status", "log entries"))
  cat(strrep("-", 70), "\n")
  for (i in seq_len(nrow(summary_dt))) {
    row <- summary_dt[i]
    icon <- if (row$status == "success") "✓" else "✗"
    cat(sprintf("  %s %-19s  %-10s  %d\n",
                icon, row$measure, row$status, row$n_log_entries))
  }
  cat(strrep("-", 70), "\n")

  cat(sprintf("\n  ✓ Successful : %d\n", sum(summary_dt$status == "success")))
  cat(sprintf("  ✗ Errors     : %d\n", sum(summary_dt$status == "error")))

  if (any(summary_dt$status == "error")) {
    cat("\n  Error details:\n")
    error_rows <- update_steps[event == "error"]
    for (i in seq_len(nrow(error_rows))) {
      cat(sprintf("    [%s] %s\n", error_rows$measure[i], error_rows$message[i]))
    }
  }

  cat(sprintf("\nTotal pipeline time: %.2f sec\n\n", total_time))

  if (any(summary_dt$status == "error")) {
    cat("⚠ Some measures failed.\n")
  } else {
    cat("✓ All measures completed successfully.\n")
  }

  invisible(summary_dt)
}