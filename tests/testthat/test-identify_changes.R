test_that("compare_aux_version_ids() returns consistent structure and metadata", {
  new_dt <- data.table::data.table(id = c("A", "B"), value = c(10, 20))
  old_dt <- data.table::data.table(id = c("A", "B"), value = c(10, 15))

  testthat::local_mocked_bindings(
    get_from_auxenv = function(object_name) {
      if (identical(object_name, "wrk_release")) {
        return(list(release = "20260401", identity = "TEST"))
      }
      if (identical(object_name, "aux_data_path")) {
        return("root/aux_data/20260401_TEST")
      }
      NULL
    },
    .package = "pipaux"
  )

  testthat::local_mocked_bindings(
    load_aux_data = function(measure,
                             version = NULL,
                             format = "qs2",
                             verbose = getOption("pipload.verbose", TRUE),
                             ppp_defaults = TRUE) {
      if (identical(version, "new_hash")) {
        return(data.table::copy(new_dt))
      }
      if (identical(version, "old_hash")) {
        return(data.table::copy(old_dt))
      }
      stop("unknown version")
    },
    .package = "pipload"
  )

  testthat::local_mocked_bindings(
    st_get_pk = function(x) "id",
    .package = "stamp"
  )

  testthat::local_mocked_bindings(
    myrror = function(dfx,
                      dfy,
                      by,
                      compare_type,
                      compare_values,
                      extract_diff_values,
                      interactive,
                      verbose) {
      list(dummy = TRUE)
    },
    extract_diff_table = function(myrror_object,
                                  by,
                                  output,
                                  interactive) {
      data.table::data.table(diff = "change_in_value", variable = "value", id = "B")
    },
    extract_diff_rows = function(myrror_object,
                                 by,
                                 output,
                                 verbose) {
      data.table::data.table(df = "dfx", id = "C")
    },
    .package = "myrror"
  )

  result <- compare_aux_version_ids(
    measure = "cpi",
    new_version_id = "new_hash",
    old_version_id = "old_hash",
    verbose = FALSE
  )

  expect_type(result, "list")
  expect_named(result, c("diff_values", "diff_rows"))
  expect_s3_class(result$diff_values, "data.table")
  expect_s3_class(result$diff_rows, "data.table")
  expect_true("change_type" %in% names(result$diff_rows))
  expect_identical(result$diff_rows$change_type[[1]], "added")

  expect_identical(attr(result, "key_cols"), "id")
  expect_identical(attr(result, "measure"), "cpi")
  expect_identical(attr(result, "new_version_id"), "new_hash")
  expect_identical(attr(result, "old_version_id"), "old_hash")
  expect_identical(attr(result, "release"), "20260401_TEST")
  expect_true(grepl("new_hash$", attr(result, "new_path")))
  expect_true(grepl("old_hash$", attr(result, "old_path")))
})

test_that("compare_aux_version_ids() validates version ID inputs", {
  expect_error(
    compare_aux_version_ids(
      measure = "cpi",
      new_version_id = "",
      old_version_id = "old_hash"
    ),
    "new_version_id"
  )

  expect_error(
    compare_aux_version_ids(
      measure = "cpi",
      new_version_id = "new_hash",
      old_version_id = NA_character_
    ),
    "old_version_id"
  )
})
