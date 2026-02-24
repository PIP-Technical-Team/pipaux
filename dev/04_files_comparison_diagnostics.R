# =====================================================================
# RELEASE + VINTAGE COMPARISON DIAGNOSTICS
# =====================================================================
# Usage:
#   source("dev/04_release_comparison_diagnostics.R")
#   run_release_diagnostics()
#   run_vintage_diagnostics()
# =====================================================================

if (!interactive()) {
  stop("Interactive use only.")
}

run_release_diagnostics <- function(
  measures,
  old_release,
  owner = "RossanaTat"
) {

  results <- data.table::data.table(
    measure = character(),
    status = character(),
    value_changes = integer(),
    row_changes = integer(),
    elapsed_sec = numeric()
  )

  for (m in measures) {

    start <- Sys.time()
    status <- "no_change"
    val_n <- 0L
    row_n <- 0L

    res <- tryCatch({
      get_aux_changes(
        measure = m,
        old_release = old_release,
        verbose = FALSE
      )
    }, error = function(e) {
      status <<- "error"
      return(NULL)
    })

    if (!is.null(res)) {
      val_n <- nrow(res$diff_values %||% data.frame())
      row_n <- nrow(res$diff_rows %||% data.frame())

      if (val_n > 0 & row_n > 0) status <- "row_and_value_change"
      else if (val_n > 0) status <- "value_change"
      else if (row_n > 0) status <- "row_change"
    }

    results <- rbind(
      results,
      data.table::data.table(
        measure = m,
        status = status,
        value_changes = val_n,
        row_changes = row_n,
        elapsed_sec = as.numeric(Sys.time() - start)
      )
    )
  }

  print(results)
  invisible(results)
}

run_vintage_diagnostics <- function(
  measures,
  version = -1
) {

  results <- data.table::data.table(
    measure = character(),
    status = character(),
    value_changes = integer(),
    row_changes = integer()
  )

  for (m in measures) {

    status <- "no_change"
    val_n <- 0L
    row_n <- 0L

    res <- tryCatch({
      compare_vintage_versions(
        measure = m,
        version = version,
        verbose = FALSE
      )
    }, error = function(e) {
      status <<- "error"
      return(NULL)
    })

    if (!is.null(res)) {
      val_n <- nrow(res$diff_values %||% data.frame())
      row_n <- nrow(res$diff_rows %||% data.frame())

      if (val_n > 0 & row_n > 0) status <- "row_and_value_change"
      else if (val_n > 0) status <- "value_change"
      else if (row_n > 0) status <- "row_change"
    }

    results <- rbind(
      results,
      data.table::data.table(
        measure = m,
        status = status,
        value_changes = val_n,
        row_changes = row_n
      )
    )
  }

  print(results)
  invisible(results)
}