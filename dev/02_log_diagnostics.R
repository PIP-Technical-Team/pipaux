# =====================================================================
# LOG DIAGNOSTICS
# =====================================================================

if (!interactive()) {
  stop("Interactive use only.")
}

run_log_diagnostics <- function(
  measures = c("cp", "metaregion"),
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
    owner = owner,
    log = TRUE,
    log_save = TRUE,
    verbose = FALSE
  )

  log_obj <- aux_log_last()

  if (is.null(log_obj)) {
    stop("No log object found after update.")
  }

  log_dt <- data.table::as.data.table(log_obj)

  if (!"logmeta" %in% names(log_dt)) {
    stop("Log object missing 'logmeta' column.")
  }

  cat("Log class:\n")
  print(class(log_obj))

  cat("\nTotal rows:", nrow(log_dt), "\n")

  # ------------------------------------------------------------
  # 2. Safe metadata extraction
  # ------------------------------------------------------------

  safe_extract <- function(x, field) {
    if (is.null(x)) return(NA_character_)
    x[[field]] %||% NA_character_
  }

  log_dt[, measure := vapply(logmeta, safe_extract, character(1), "measure")]
  log_dt[, step    := vapply(logmeta, safe_extract, character(1), "step")]

  cat("\nMeasures processed:\n")
  print(unique(na.omit(log_dt$measure)))

  cat("\nStep types:\n")
  print(unique(na.omit(log_dt$step)))

  # ------------------------------------------------------------
  # 3. Dependency ordering validation
  # ------------------------------------------------------------

  dependency_order <- names(
    read_dependencies(
      gh_user = "https://raw.githubusercontent.com",
      owner   = owner
    )
  )

  log_measures <- unique(na.omit(log_dt$measure))

  expected_order <- dependency_order[
    dependency_order %in% log_measures
  ]

  order_ok <- identical(log_measures, expected_order)

  cat("\nDependency order respected:", order_ok, "\n")

  # ------------------------------------------------------------
  # 4. Persistence validation
  # ------------------------------------------------------------

  log_name <- aux_log_last_name()

  loaded_log <- pipfun::log_load(
    id = log_name,
    alias = aux_meta_alias,
    overwrite = TRUE,
    verbose = FALSE
  )

  persistence_ok <-
    inherits(loaded_log, class(log_obj)[1]) &&
    nrow(loaded_log) == nrow(log_dt) &&
    all(names(loaded_log) == names(log_dt))

  cat("\nPersistence validation:", persistence_ok, "\n")

  # ------------------------------------------------------------
  # 5. Structured result
  # ------------------------------------------------------------

  result <- list(
    n_rows = nrow(log_dt),
    measures = log_measures,
    order_ok = order_ok,
    persistence_ok = persistence_ok
  )

  cat("\n", strrep("-", 70), "\n")

  if (order_ok && persistence_ok) {
    cat("✓ Log diagnostics passed.\n")
  } else {
    cat("⚠ Log diagnostics found issues.\n")
  }

  invisible(result)
}