# =====================================================================
# INTERACTIVE MASTER RUN: Run All Steps
# =====================================================================
# Purpose:
#   - Execute the full auxiliary data release workflow interactively
#   - Run updates, log diagnostics, simulations, and release comparisons
#   - Track timing and summarize results across all steps
#
# Usage:
#   source("dev/05_interactive_full_run.R")
#   results <- run_all_workflow()
#
#   # With optional arguments
#   results <- run_all_workflow(
#     measures    = c("cpi", "pfw"),
#     owner       = "RossanaTat",
#     old_release = "20260202_TEST",
#     version     = -1,
#     log_save    = TRUE,
#     verbose     = TRUE
#   )
#
# Expected output:
#   - Step-by-step console output from each diagnostic function
#   - Per-step elapsed time
#   - Final workflow summary with total elapsed time
#   - Invisibly returns a named list with results from all four steps:
#       $update_results, $log_diagnostics,
#       $simulation_results, $release_diagnostics, $vintage_diagnostics
#
# Notes:
#   - old_release must be provided for run_release_diagnostics() to run;
#     if NULL, that sub-step is skipped with a warning
#   - version = -1 compares latest vs. previous vintage (default)
# =====================================================================

if (!interactive()) stop("This file is intended for interactive use only.")

run_all_workflow <- function(
  measures    = NULL,
  owner       = "RossanaTat",
  old_release = NULL,
  version     = -1L,
  log_save    = TRUE,
  verbose     = TRUE
) {

  start_all <- Sys.time()
  workflow_results <- list()

  # -------------------------------------------------------------------
  # Step 1: Run dependency-aware update
  # -------------------------------------------------------------------
  cat("\n", strrep("=", 80), "\n")
  cat("STEP 1: Updating auxiliary measures\n")
  cat(strrep("=", 80), "\n\n")

  source("dev/01_dependency_and_update_runner.R", local = TRUE)

  update_start <- Sys.time()
  update_results <- run_ordered_update_diagnostics(
    measures = measures,
    owner    = owner,
    log_save = log_save,
    verbose  = verbose
  )
  workflow_results$update_results <- update_results
  update_elapsed <- as.numeric(Sys.time() - update_start)

  cat(sprintf("\nStep 1 completed in %.2f seconds\n\n", update_elapsed))

  # -------------------------------------------------------------------
  # Step 2: Log diagnostics
  # -------------------------------------------------------------------
  cat("\n", strrep("=", 80), "\n")
  cat("STEP 2: Log diagnostics\n")
  cat(strrep("=", 80), "\n\n")

  source("dev/02_log_diagnostics.R", local = TRUE)

  log_start <- Sys.time()
  log_results <- run_log_diagnostics(
    measures = measures,
    owner    = owner
  )
  workflow_results$log_diagnostics <- log_results
  log_elapsed <- as.numeric(Sys.time() - log_start)

  cat(sprintf("\nStep 2 completed in %.2f seconds\n\n", log_elapsed))

  # -------------------------------------------------------------------
  # Step 3: Simulations
  # -------------------------------------------------------------------
  cat("\n", strrep("=", 80), "\n")
  cat("STEP 3: Simulations\n")
  cat(strrep("=", 80), "\n\n")

  source("dev/03_simulation_tools.R", local = TRUE)

  sim_start <- Sys.time()
  sim_results <- run_simulation_diagnostics(
    measures = measures   # NULL falls back to default inside 03_
  )
  workflow_results$simulation_results <- sim_results
  sim_elapsed <- as.numeric(Sys.time() - sim_start)

  cat(sprintf("\nStep 3 completed in %.2f seconds\n\n", sim_elapsed))

  # -------------------------------------------------------------------
  # Step 4: Release & vintage comparison diagnostics
  # -------------------------------------------------------------------
  cat("\n", strrep("=", 80), "\n")
  cat("STEP 4: Release & vintage comparison diagnostics\n")
  cat(strrep("=", 80), "\n\n")

  source("dev/04_files_comparison_diagnostics.R", local = TRUE)

  diag_start <- Sys.time()

  if (is.null(old_release)) {
    warning("old_release is NULL -- skipping run_release_diagnostics().")
    workflow_results$release_diagnostics <- NULL
  } else {
    release_diag <- run_release_diagnostics(
      measures    = measures,
      old_release = old_release,
      owner       = owner
    )
    workflow_results$release_diagnostics <- release_diag
  }

  vintage_diag <- run_vintage_diagnostics(
    measures = measures,
    version  = version
  )
  workflow_results$vintage_diagnostics <- vintage_diag

  diag_elapsed <- as.numeric(Sys.time() - diag_start)
  cat(sprintf("\nStep 4 completed in %.2f seconds\n\n", diag_elapsed))

  # -------------------------------------------------------------------
  # Full workflow summary
  # -------------------------------------------------------------------
  total_elapsed <- as.numeric(Sys.time() - start_all)
  cat("\n", strrep("=", 80), "\n")
  cat("FULL WORKFLOW COMPLETED\n")
  cat(strrep("=", 80), "\n\n")

  cat(sprintf("Step 1 (Update) elapsed:                      %.2f s\n", update_elapsed))
  cat(sprintf("Step 2 (Log diagnostics) elapsed:             %.2f s\n", log_elapsed))
  cat(sprintf("Step 3 (Simulations) elapsed:                 %.2f s\n", sim_elapsed))
  cat(sprintf("Step 4 (Release/Vintage diagnostics) elapsed: %.2f s\n", diag_elapsed))
  cat(sprintf("TOTAL workflow elapsed:                       %.2f s\n\n", total_elapsed))

  invisible(workflow_results)
}