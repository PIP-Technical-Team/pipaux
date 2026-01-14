library(mockr)

test_that("Leaf node update works", {
  called <- FALSE
  with_mock(
    `pipaux::aux_country_list` = function(...) { called <<- TRUE },
    {
      expect_no_error(aux_fun("country_list", log = FALSE))
      expect_true(called)
    }
  )
})

test_that("Root node update with dependencies works", {
  calls <- character()
  with_mock(
    `pipaux::aux_country_list` = function(...) { calls <<- c(calls, "country_list") },
    `pipaux::aux_countries` = function(...) { calls <<- c(calls, "countries") },
    {
      calls <<- character()
      expect_no_error(aux_fun("countries", log = FALSE))
      expect_true(all(c("country_list", "countries") %in% calls))
      expect_equal(sum(calls == "country_list"), 1) # Only once
    }
  )
})

test_that("Missing aux function triggers error", {
  expect_error(aux_fun("nonexistent_measure", log = FALSE), "does not exist")
})

test_that("Dependency failure halts or logs as expected", {
  with_mock(
    `pipaux::aux_country_list` = function(...) stop("Simulated error"),
    {
      # Should halt if halt_on_dep_fail = TRUE
      expect_error(aux_fun("countries", halt_on_dep_fail = TRUE, log = FALSE), "Simulated error")
      # Should log and continue if halt_on_dep_fail = FALSE
      expect_no_error(aux_fun("countries", halt_on_dep_fail = FALSE, log = FALSE))
    }
  )
})

test_that("Logging output is correct", {
  pipfun::log_init("pipaux_update_log", overwrite = TRUE)
  with_mock(
    `pipaux::aux_country_list` = function(...) NULL,
    {
      aux_fun("country_list", log = TRUE)
      log <- pipfun::log_get("pipaux_update_log")
      expect_true(any(log$event == "update"))
      expect_true(any(grepl("country_list", log$message)))
    }
  )
})

# Optional: Cycle prevention test (if you ever allow cycles in dependencies)
# test_that("Cycle prevention works", { ... })