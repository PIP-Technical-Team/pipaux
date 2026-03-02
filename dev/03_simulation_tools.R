# =====================================================================
# SIMULATION TOOLS
# =====================================================================
# Usage:
#   source("dev/03_simulation_tools.R")
#   run_simulation_diagnostics()
# =====================================================================

if (!interactive()) {
  stop("Interactive use only.")
}

run_simulation_diagnostics <- function(
  measures = c("cpi", "pop", "pfw", "gdp", "pce")
) {

  cat("\n", strrep("=", 70), "\n")
  cat("SIMULATION DIAGNOSTICS\n")
  cat(strrep("=", 70), "\n\n")

  default_config <- list(drop = c(1), indices = c(10))

  simulation_config <- list(
    cpi = list(drop = c(1, 5), indices = c(20, 50)),
    pop = list(drop = c(2, 8), indices = c(15, 45)),
    pfw = list(drop = c(3),    indices = c(10)),
    gdp = list(drop = c(4),    indices = c(25)),
    pce = list(drop = c(1),    indices = c(18))
  )

  results <- data.table::data.table(
    measure = character(),
    status = character(),
    elapsed_sec = numeric()
  )

  for (m in measures) {

    cat(sprintf("[%s]\n", m))
    start <- Sys.time()
    status <- "success"

    cfg <- if (!is.null(simulation_config[[m]])) {
      simulation_config[[m]]
    } else {
      cat(sprintf("  ℹ No config found for '%s', using defaults.\n", m))
      default_config
    }

    tryCatch({
      simulate_changes(
        measure = m,
        seed    = 123,
        drop    = cfg$drop,
        indices = cfg$indices,
        verbose = FALSE
      )
      cat("  ✓ simulated\n")
    }, error = function(e) {
      status <<- "error"
      cat("  ✗ ERROR:", e$message, "\n")
    })

    elapsed <- as.numeric(Sys.time() - start)

    results <- rbind(
      results,
      data.table::data.table(
        measure = m,
        status = status,
        elapsed_sec = elapsed
      )
    )
  }

  cat("\nSummary:\n")
  print(results)

  invisible(results)
}