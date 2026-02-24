# =====================================================================
# INTERACTIVE MASTER RUN: Run All Steps
# =====================================================================
# Purpose:
#   - Execute the full auxiliary data release workflow interactively
#   - Run updates, log diagnostics, simulations, and release comparisons
#   - Track timing and summarize results
#
# Usage:
#   source("dev/05_interactive_run_all.R")
#   results <- run_all_workflow()
# =====================================================================

if (!interactive()) stop("This file is intended for interactive use only.")

run_all_workflow <- function(
  measures = NULL,
  owner = "RossanaTat",
  sim_config = NULL,
  old_release = NULL,
  verbose = TRUE
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
  update_results <- run_dependency_update(
    measures = measures,
    owner = owner
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
    owner = owner
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
  if (is.null(sim_config)) {
    sim_config <- list() # default simulation settings can be defined in 03
  }
  sim_results <- run_simulations(
    measures = measures,
    sim_config = sim_config
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
  
  source("dev/04_release_comparison_diagnostics.R", local = TRUE)
  
  diag_start <- Sys.time()
  
  release_diag <- run_release_comparison_diagnostics(
    measures = measures,
    old_release = old_release,
    owner = owner
  )
  workflow_results$release_diagnostics <- release_diag
  
  vintage_diag <- run_vintage_comparison_diagnostics(
    measures = measures
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
  
  cat(sprintf("Step 1 (Update) elapsed: %.2f s\n", update_elapsed))
  cat(sprintf("Step 2 (Log diagnostics) elapsed: %.2f s\n", log_elapsed))
  cat(sprintf("Step 3 (Simulations) elapsed: %.2f s\n", sim_elapsed))
  cat(sprintf("Step 4 (Release/Vintage diagnostics) elapsed: %.2f s\n", diag_elapsed))
  cat(sprintf("TOTAL workflow elapsed: %.2f s\n\n", total_elapsed))
  
  return(invisible(workflow_results))
}