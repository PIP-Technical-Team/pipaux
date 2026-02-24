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
  log_save = FALSE,
  verbose = TRUE
) {

  cat("\n", strrep("=", 70), "\n")
  cat("ORDERED UPDATE ENGINE DIAGNOSTICS\n")
  cat(strrep("=", 70), "\n\n")

  if (is.null(owner)) {
    stop("Owner must be set via option 'pipfun.ghowner' or provided.")
  }

  # ------------------------------------------------------------
  # 1. Run update engine
  # ------------------------------------------------------------

  start_total <- Sys.time()

  update_aux_measures(
    measures = measures,
    owner = owner,
    tag = tag,
    log = TRUE,
    log_overwrite = TRUE,
    verbose = verbose,
    halt_on_dep_fail = FALSE,
    log_save = log_save
  )

  total_time <- as.numeric(Sys.time() - start_total)

  # ------------------------------------------------------------
  # 2. Retrieve log
  # ------------------------------------------------------------

  log_obj <- aux_log_last()

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

  # Extract measure + step metadata safely
  log_dt[, measure := sapply(logmeta, function(x) x$measure %||% NA_character_)]
  log_dt[, step    := sapply(logmeta, function(x) x$step %||% NA_character_)]

  # Keep only update-level steps
  update_steps <- log_dt[!is.na(measure)]

  # Derive status by measure
  summary_dt <- update_steps[, .(
    n_events = .N,
    errors = sum(event == "error", na.rm = TRUE)
  ), by = measure]

  summary_dt[, status := ifelse(errors > 0, "error", "success")]

  # Optional timing extraction if timestamps exist
  if ("timestamp" %in% names(update_steps)) {

    timing_dt <- update_steps[, .(
      start_time = min(timestamp),
      end_time   = max(timestamp)
    ), by = measure]

    timing_dt[, elapsed_sec :=
                as.numeric(difftime(end_time, start_time, units = "secs"))]

    summary_dt <- merge(
      summary_dt,
      timing_dt[, .(measure, elapsed_sec)],
      by = "measure",
      all.x = TRUE
    )

  } else {
    summary_dt[, elapsed_sec := NA_real_]
  }

  # Order by dependency execution sequence
  summary_dt[, order := seq_len(.N)]
  data.table::setcolorder(
    summary_dt,
    c("order", "measure", "status", "elapsed_sec", "errors", "n_events")
  )

  # ------------------------------------------------------------
  # 4. Print clean summary
  # ------------------------------------------------------------

  cat("\nExecution Summary:\n")
  print(summary_dt)

  cat("\nTotal pipeline time: ",
      sprintf("%.2f sec", total_time), "\n\n")

  if (any(summary_dt$status == "error")) {
    cat("⚠ Some measures failed.\n")
  } else {
    cat("✓ All measures completed successfully.\n")
  }

  invisible(summary_dt)
}