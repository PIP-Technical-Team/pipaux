run_isolated_stamp_test <- function(file) {
  file <- normalizePath(file, winslash = "/", mustWork = TRUE)
  tmp_base <- normalizePath(tempdir(), winslash = "/", mustWork = TRUE)
  if (grepl("^[CcYy]:", tmp_base)) {
    stop("Start R with TEMP, TMP, and TMPDIR set to an approved E-drive directory.")
  }
  sandbox <- tempfile("pipaux-stamp-process-", tmpdir = tmp_base)
  dir.create(sandbox)
  on.exit(unlink(sandbox, recursive = TRUE), add = TRUE)
  cache <- file.path(sandbox, "cache")
  dir.create(cache)

  package_path <- getNamespaceInfo(asNamespace("pipaux"), "path")
  callr::r(
    function(file, package_path, source_package, sandbox) {
      options(pipaux.working_dir = sandbox)
      library(testthat)
      if (source_package) {
        pkgload::load_all(package_path, quiet = TRUE)
      } else {
        library("pipaux", lib.loc = dirname(package_path))
      }
      results <- testthat::test_file(file, package = "pipaux",
        reporter = "summary", stop_on_failure = TRUE, stop_on_warning = TRUE)
      counts <- as.data.frame(results)
      cat("Child assertions:", sum(counts$passed), "passed;",
        sum(counts$failed), "failed;", sum(counts$warning), "warnings;",
        sum(counts$skipped), "skipped\n")
      TRUE
    },
    args = list(file, package_path, pkgload::is_dev_package("pipaux"), sandbox),
    libpath = .libPaths(),
    env = c(callr::rcmd_safe_env(),
      TEMP = sandbox, TMP = sandbox, TMPDIR = sandbox,
      HOME = sandbox, R_USER = sandbox,
      XDG_CACHE_HOME = cache, R_USER_CACHE_DIR = cache,
      PIPAUX_STAMP_TEST_CHILD = basename(file)),
    user_profile = FALSE,
    system_profile = FALSE,
    show = TRUE
  )
}
