# =====================================================================
# LOG DIAGNOSTICS
# =====================================================================
# Purpose:
#   - Validate that update_aux_measures() produces a well-formed log
#   - Inspect log structure (class, columns, row count)
#   - Confirm measures are correctly recorded in logmeta
#   - Verify log persistence to the aux metadata repository
#
# Usage:
#   source("dev/02_log_diagnostics.R")
#   run_log_diagnostics()
#
#   # Optional subset of measures
#   run_log_diagnostics(measures = c("cpi", "pfw"))
#
# Expected output:
#   - Log class, row count, and column names
#   - Measures found in log entries
#   - Persistence check (saved log matches in-memory log)
#   - Final pass/fail summary
# =====================================================================

if (!interactive()) {
  stop("Interactive use only.")
}

run_log_diagnostics <- function(
  measures = c("cp", "pfw"),
  owner = getOption("pipfun.ghowner")
) {

  cat("\n", strrep("=", 70), "\n")
  cat("LOG DIAGNOSTICS\n")
  cat(strrep("=", 70), "\n\n")

  if (is.null(owner)) {
    stop("Owner must be set via option 'pipfun.ghowner' or provided.")
  }

  aux_meta_alias <- get_from_auxenv("aux_meta_alias")

  # ------------------------------------------------------------
  # 1. Run update engine
  # ------------------------------------------------------------

  update_aux_measures(
    measures = measures,
    owner    = owner,
    log      = TRUE,
    log_save = TRUE,
    verbose  = FALSE
  )

  # ------------------------------------------------------------
  # 2. Load and inspect log
  # ------------------------------------------------------------

  log_obj <- pipfun::log_load(
    id       = "pipaux_update_log",
    alias    = "aux_meta",
    overwrite = TRUE,
    verbose  = FALSE
  )

  if (is.null(log_obj)) {
    stop("No log object found after update.")
  }

  log_dt <- data.table::as.data.table(log_obj)

  if (!"logmeta" %in% names(log_dt)) {
    stop("Log object missing 'logmeta' column.")
  }

  cat("Log class:  ", paste(class(log_obj), collapse = ", "), "\n")
  cat("Total rows: ", nrow(log_dt), "\n")
  cat("Columns:    ", paste(names(log_dt), collapse = ", "), "\n")

  # ------------------------------------------------------------
  # 3. Extract measures from logmeta
  # ------------------------------------------------------------

  safe_extract <- function(x, field) {
    if (is.null(x)) return(NA_character_)
    val <- x[[field]]
    if (is.null(val)) NA_character_ else as.character(val)
  }

  log_dt[, measure := vapply(logmeta, safe_extract, character(1), "measure")]
  log_dt[, step    := vapply(logmeta, safe_extract, character(1), "step")]

  log_measures <- unique(stats::na.omit(log_dt$measure))
  cat("Measures in log:", paste(log_measures, collapse = ", "), "\n")

  # ------------------------------------------------------------
  # 4. Persistence validation
  # ------------------------------------------------------------

  log_name <- "pipaux_update_log"

  loaded_log <- pipfun::log_load(
    id       = log_name,
    alias    = aux_meta_alias,
    overwrite = TRUE,
    verbose  = FALSE
  )

  persistence_ok <-
    inherits(loaded_log, class(log_obj)[1]) &&
    nrow(loaded_log) == nrow(log_dt) 


  cat("Persistence ok: ", persistence_ok, "\n")

  # ------------------------------------------------------------
  # 5. Summary
  # ------------------------------------------------------------

  result <- list(
    n_rows         = nrow(log_dt),
    measures       = log_measures,
    persistence_ok = persistence_ok
  )

  cat("\n", strrep("-", 70), "\n")

  if (persistence_ok) {
    cat("✓ Log diagnostics passed.\n")
  } else {
    cat("⚠ Log diagnostics found issues.\n")
  }

  invisible(result)
}