# Focused M2 Phase 2 Step 4 (L-aux / V7) verification for CPI, PPP, and PFW

source("helper-stamp-process.R", local = TRUE)

if (Sys.getenv("PIPAUX_STAMP_TEST_CHILD") != "test-m2-stamp-cpi-ppp-pfw.R") {
  test_that("M2 storage tests leave caller stamp state unchanged", {
    before <- list(stamp::st_alias_list(), stamp::st_alias_get(), stamp::st_opts_get())
    expect_true(run_isolated_stamp_test("test-m2-stamp-cpi-ppp-pfw.R"))
    expect_identical(
      list(stamp::st_alias_list(), stamp::st_alias_get(), stamp::st_opts_get()), before
    )
  })
} else {

with_isolated_aux_stamp <- function(code) {
  tmp_base <- Sys.getenv("TMPDIR", unset = "")
  if (!nzchar(tmp_base)) {
    tmp_base <- Sys.getenv("TMP", unset = tempdir())
  }
  if (grepl("^[Cc]:", tmp_base) && dir.exists("E:/PovcalNet/01.personal/wb384996/PIP/pipaux/.tmp")) {
    tmp_base <- "E:/PovcalNet/01.personal/wb384996/PIP/pipaux/.tmp"
  }
  if (grepl("^[Cc]:", tmp_base)) {
    cli::cli_abort("Refusing to write temporary test artifacts to C: drive ({.path {tmp_base}}).")
  }

  tmp_root <- fs::path(
    tmp_base,
    paste0("pipaux-m2-stamp-", Sys.getpid(), "-", sample.int(1e6, 1))
  )
  fs::dir_create(tmp_root, recurse = TRUE)

  old_alias <- get_from_auxenv("aux_alias")
  old_path  <- get_from_auxenv("aux_data_path")
  old_wrk   <- get_from_auxenv("wrk_release")

  on.exit({
    rlang::env_poke(.pipaux, "aux_alias", old_alias)
    rlang::env_poke(.pipaux, "aux_data_path", old_path)
    rlang::env_poke(.pipaux, "wrk_release", old_wrk)
    if (fs::dir_exists(tmp_root)) {
      fs::dir_delete(tmp_root)
    }
  }, add = TRUE)

  stamp::st_opts_reset()
  stamp::st_opts(
    versioning = "content",
    code_hash = TRUE,
    verify_on_load = TRUE
  )

  alias_name <- basename(tmp_root)
  stamp::st_init(tmp_root, alias = alias_name, verbose = FALSE)

  rlang::env_poke(.pipaux, "aux_alias", alias_name)
  rlang::env_poke(.pipaux, "aux_data_path", tmp_root)
  rlang::env_poke(
    .pipaux,
    "wrk_release",
    list(release = "20260324", identity = "TEST", version_id = "20260324_TEST")
  )

  testthat::local_mocked_bindings(
    get_wrk_release = function(verbose = FALSE) {
      list(release = "20260324", identity = "TEST", version_id = "20260324_TEST")
    },
    get_pip_folders = function(folder = "aux_data", verbose = FALSE, ...) {
      tmp_root
    },
    .package = "pipfun"
  )

  # Seed minimal country_list in the isolated stamp root for aux_*_clean()
  cl <- data.table::data.table(
    country_code = c("AAA", "BBB"),
    region_code  = c("SSA", "EAP")
  )
  pip_aux_save(cl, id = "country_list", pk = "country_code", verbose = FALSE)

  force(code)
}

make_valid_cpi_raw_fixture <- function(sha = "sha_cpi_v1", cpi2017_first = 1.2) {
  dt <- data.table::data.table(
    region           = c("SSA", "EAP"),
    code             = c("AAA", "BBB"),
    countryname      = c("Country A", "Country B"),
    year             = c(2010, 2011),
    survname         = c("Survey A", "Survey B"),
    ref_year         = c(2010.0, 2011.0),
    cpi_domain       = c("National", "Urban/Rural"),
    cpi_domain_value = c(0, 1),
    cpi_replication  = c(1, 1),
    cpi_data_level   = c(2, 1),
    cpi2005          = c(TRUE, FALSE),
    cpi2011          = c(1.0, 1.1),
    cpi2017          = c(cpi2017_first, 1.3),
    cpi2021          = c(1.4, 1.5),
    cpi2011_unadj    = c(1.0, 1.1),
    cpi2017_unadj    = c(cpi2017_first, 1.3),
    cpi2021_unadj    = c(1.4, 1.5),
    version          = c("v1", "v1"),
    comparability    = c(1, 1),
    cur_adj          = c(1.0, 1.0),
    survey_coverage  = c("national", "urban"),
    comparable       = c(1, 1),
    change_cpi2017   = c(0, 0),
    change_icp2017   = c(0, 0),
    change_cpi2011   = c(0, 1),
    change_icp2011   = c(0, 0),
    cpi_id           = c("CPI_v1", "CPI_v1"),
    cpi_domain_var   = c("var1", "var2"),
    cpi2011_SM21     = c(1.0, 1.1),
    cpi2017_SM21     = c(1.2, 1.3)
  )
  data.table::setattr(dt, "gh", list(gh_raw_sha = sha, branch = "20260324_TEST"))
  dt
}

make_valid_ppp_raw_fixture <- function(sha = "sha_ppp_v1", add_v2_2017 = FALSE) {
  dt <- data.table::data.table(
    CountryName      = c("Country A", "Country B"),
    code             = c("AAA", "BBB"),
    CoverageType     = c("National", "Urban"),
    ppp_2005_v1_v1   = c(1.0, 2.0),
    ppp_2011_v1_v1   = c(1.1, 2.1),
    ppp_2011_v1_v2   = c(1.2, 2.2),
    ppp_2011_v2_v1   = c(1.3, 2.3),
    ppp_2011_v2_v2   = c(1.35, 2.35),
    ppp_2017_v1_v1   = c(1.4, 2.4),
    ppp_2017_v1_v2   = c(1.5, 2.5),
    ppp_2021_v1_v1   = c(1.6, 2.6),
    ppp_2021_v1_v2   = c(1.7, 2.7),
    source_ppp_2011  = c(1, 1),
    source_ppp_2005  = c(1, 1),
    Time             = c("2011", "2017"),
    datalevel        = c(2, 1),
    ppp_domain       = c(1, 2),
    ppp_domain_value = c(1, 2),
    ppp_domain_var   = c("var1", "var2"),
    oldicp2005       = c(1.0, 2.0),
    oldicp2011       = c(1.1, 2.1),
    Seriesname       = c("Series A", "Series B"),
    note_may192020   = c("", "")
  )
  if (isTRUE(add_v2_2017)) {
    dt[, ppp_2017_v2_v1 := c(1.85, 2.85)]
  }
  data.table::setattr(dt, "gh", list(gh_raw_sha = sha, branch = "20260324_TEST"))
  dt
}

make_valid_pfw_raw_fixture <- function(sha = "sha_pfw_v1") {
  dt <- data.table::data.table(
    region             = c("SSA", "EAP"),
    code               = c("AAA", "BBB"),
    reg_pcn            = c("SSA", "EAP"),
    ctryname           = c("Country A", "Country B"),
    year               = c(2010, 2011),
    surveyid_year      = c(2010L, 2011L),
    timewp             = c(1.0, 2.0),
    fieldwork          = c(2010.0, 2011.0),
    survname           = c("Survey A", "Survey B"),
    link               = c("", ""),
    altname            = c("", ""),
    survey_time        = c("2010", "2011"),
    wbint_link         = c(1, 1),
    wbext_link         = c(1, 1),
    alt_link           = c(0, 0),
    pip_meta           = c(1, 1),
    surv_title         = c("Title A", "Title B"),
    surv_producer      = c("Producer A", "Producer B"),
    survey_coverage    = c("N", "U"),
    datatype           = c("C", "I"),
    use_imputed        = c(0, 1),
    use_microdata      = c(1, 0),
    use_bin            = c(0, 0),
    use_groupdata      = c(0, 1),
    rep_year           = c(2010, 2011),
    comparability      = c(1, 2),
    comp_note          = c("", ""),
    preferable         = c("", ""),
    display_cp         = c(1, 0),
    fieldwork_range    = c("", ""),
    ref_year           = c(2010.5, 2011.5),
    newref             = c("", ""),
    ref_year_des       = c(1.0, 2.0),
    wf_baseprice       = c("", ""),
    wf_baseprice_note  = c("", ""),
    wf_baseprice_des   = c(-9, -8),
    wf_spatial_des     = c(0, 1),
    wf_spatial_var     = c("", ""),
    cpi_replication    = c(-9, 1),
    cpi_domain         = c(1, 2),
    cpi_domain_var     = c("", ""),
    wf_currency_des    = c(0, 2),
    ppp_replication    = c(-9, 1),
    ppp_domain         = c(1, 2),
    ppp_domain_var     = c("", ""),
    wf_add_temp_des    = c(-9, 0),
    wf_add_temp_var    = c(0, 1),
    wf_add_spatial_des = c(-9, 0),
    wf_add_spatial_var = c(0, 1),
    tosplit            = c(NA_real_, 1),
    tosplit_var        = c("", ""),
    inpovcal           = c(1, 1),
    oth_welfare1_type  = c("I", ""),
    oth_welfare1_var   = c("", ""),
    gdp_domain         = c(1, 2),
    pce_domain         = c(1, 2),
    pop_domain         = c(1, 2),
    pfw_id             = c("id1", "id2")
  )
  data.table::setattr(dt, "gh", list(gh_raw_sha = sha, branch = "20260324_TEST"))
  dt
}

test_that("L-aux (1): valid CPI, PPP, and PFW fixtures save and read back with exact keys, classes, and metadata pins", {
  with_isolated_aux_stamp({
    alias_name <- get_from_auxenv("aux_alias")

    seen_branches <- character()
    testthat::local_mocked_bindings(
      load_from_gh = function(measure, branch = NULL, ...) {
        seen_branches <<- c(seen_branches, paste0(measure, ":", branch))
        switch(
          measure,
          cpi = make_valid_cpi_raw_fixture(sha = "sha_cpi_v1"),
          ppp = make_valid_ppp_raw_fixture(sha = "sha_ppp_v1"),
          pfw = make_valid_pfw_raw_fixture(sha = "sha_pfw_v1"),
          cli::cli_abort("Unexpected measure {.val {measure}}")
        )
      },
      .package = "pipfun"
    )

    cpi_pin <- aux_cpi_update(branch = "main", detail = FALSE, verbose = FALSE)
    ppp_pin <- aux_ppp_update(branch = "main", detail = FALSE, verbose = FALSE)
    pfw_pin <- aux_pfw_update(branch = "main", detail = FALSE, verbose = FALSE)

    expect_identical(seen_branches, c("cpi:main", "ppp:main", "pfw:main"))

    expect_false(cpi_pin$skipped)
    expect_false(ppp_pin$skipped)
    expect_false(pfw_pin$skipped)
    expect_true(nzchar(cpi_pin$version_id))
    expect_true(nzchar(ppp_pin$version_id))
    expect_true(nzchar(pfw_pin$version_id))

    # Read back through pipload::load_aux_data() / aux_*("load")
    cpi_dt <- aux_cpi(action = "load", verbose = FALSE)
    ppp_dt <- aux_ppp(action = "load", verbose = FALSE)
    pfw_dt <- aux_pfw(action = "load", verbose = FALSE)

    expect_s3_class(cpi_dt, "data.table")
    expect_s3_class(ppp_dt, "data.table")
    expect_s3_class(pfw_dt, "data.table")

    # Exact primary keys (Contracts E1-E3)
    expected_cpi_pk <- c("country_code", "year", "survey_acronym", "reporting_level", "cpi_year")
    expected_ppp_pk <- c("country_code", "reporting_level", "ppp_year", "release_version", "adaptation_version")
    expected_pfw_pk <- c("country_code", "surveyid_year", "welfare_type", "is_alt_welf")

    expect_identical(stamp::st_get_pk(cpi_dt), expected_cpi_pk)
    expect_identical(data.table::key(cpi_dt), expected_cpi_pk)
    expect_identical(stamp::st_get_pk(ppp_dt), expected_ppp_pk)
    expect_identical(data.table::key(ppp_dt), expected_ppp_pk)
    expect_identical(stamp::st_get_pk(pfw_dt), expected_pfw_pk)
    expect_identical(data.table::key(pfw_dt), expected_pfw_pk)

    # Column classes
    expect_type(cpi_dt$country_code, "character")
    expect_type(cpi_dt$year, "integer")
    expect_type(cpi_dt$survey_acronym, "character")
    expect_type(cpi_dt$reporting_level, "character")
    expect_type(cpi_dt$cpi_year, "integer")
    expect_type(cpi_dt$cpi_value, "double")
    expect_type(cpi_dt$survey_year, "double")
    expect_type(cpi_dt$ccf, "double")

    expect_type(ppp_dt$country_code, "character")
    expect_type(ppp_dt$reporting_level, "character")
    expect_type(ppp_dt$ppp_year, "double")
    expect_type(ppp_dt$release_version, "character")
    expect_type(ppp_dt$adaptation_version, "character")
    expect_type(ppp_dt$ppp, "double")
    expect_type(ppp_dt$ppp_default, "logical")
    expect_type(ppp_dt$ppp_default_by_year, "logical")

    expect_type(pfw_dt$country_code, "character")
    expect_type(pfw_dt$surveyid_year, "integer")
    expect_type(pfw_dt$welfare_type, "character")
    expect_type(pfw_dt$is_alt_welf, "logical")
    expect_equal(nrow(pfw_dt), 3L) # AAA main + AAA alt + BBB main

    # Sidecar metadata pins (gh, code_hash, code_label, pk)
    cpi_meta <- stamp::st_read_sidecar("cpi.qs2", alias = alias_name)
    ppp_meta <- stamp::st_read_sidecar("ppp.qs2", alias = alias_name)
    pfw_meta <- stamp::st_read_sidecar("pfw.qs2", alias = alias_name)

    expect_identical(cpi_meta$gh$gh_raw_sha, "sha_cpi_v1")
    expect_identical(cpi_meta$code_label, "aux_cpi_update")
    expect_identical(cpi_meta$code_hash, stamp:::st_hash_code(aux_cpi_update))
    expect_identical(stamp::st_get_pk(cpi_meta), expected_cpi_pk)

    expect_identical(ppp_meta$gh$gh_raw_sha, "sha_ppp_v1")
    expect_identical(ppp_meta$code_label, "aux_ppp_update")
    expect_identical(ppp_meta$code_hash, stamp:::st_hash_code(aux_ppp_update))
    expect_identical(stamp::st_get_pk(ppp_meta), expected_ppp_pk)

    expect_identical(pfw_meta$gh$gh_raw_sha, "sha_pfw_v1")
    expect_identical(pfw_meta$code_label, "aux_pfw_update")
    expect_identical(pfw_meta$code_hash, stamp:::st_hash_code(aux_pfw_update))
    expect_identical(stamp::st_get_pk(pfw_meta), expected_pfw_pk)
  })
})

test_that("L-aux (2): duplicate keys on persisted CPI, PPP, and PFW schemas fail before saving", {
  with_isolated_aux_stamp({
    alias_name <- get_from_auxenv("aux_alias")

    cpi_pk <- c("country_code", "year", "survey_acronym", "reporting_level", "cpi_year")
    ppp_pk <- c("country_code", "reporting_level", "ppp_year", "release_version", "adaptation_version")
    pfw_pk <- c("country_code", "surveyid_year", "welfare_type", "is_alt_welf")

    dup_cpi <- data.table::data.table(
      country_code    = c("AAA", "AAA"),
      year            = c(2010L, 2010L),
      cpi_year        = c(2017L, 2017L),
      cpi_value       = c(1.1, 1.2),
      survey_year     = c(2010.0, 2010.0),
      ccf             = c(1.0, 1.0),
      survey_acronym  = c("Survey A", "Survey A"),
      reporting_level = c("national", "national")
    )
    expect_error(cpi_validate_output(dup_cpi, detail = FALSE))
    expect_error(pip_aux_save(dup_cpi, id = "cpi", pk = cpi_pk, verbose = FALSE))
    expect_false(fs::file_exists(fs::path(get_from_auxenv("aux_data_path"), "cpi.qs2")))
    expect_false(fs::dir_exists(fs::path(get_from_auxenv("aux_data_path"), "cpi.qs2")))
    expect_false(fs::file_exists(fs::path(get_from_auxenv("aux_data_path"), "cpi.qs2", "cpi.qs2")))

    dup_ppp <- data.table::data.table(
      country_code        = c("AAA", "AAA"),
      ppp_year            = c(2017, 2017),
      release_version     = c("v1", "v1"),
      adaptation_version  = c("v2", "v2"),
      ppp                 = c(1.5, 1.6),
      ppp_default         = c(TRUE, TRUE),
      ppp_default_by_year = c(TRUE, TRUE),
      reporting_level     = c("national", "national")
    )
    expect_error(ppp_validate_output(dup_ppp, detail = FALSE))
    expect_error(pip_aux_save(dup_ppp, id = "ppp", pk = ppp_pk, verbose = FALSE))
    expect_false(fs::file_exists(fs::path(get_from_auxenv("aux_data_path"), "ppp.qs2")))
    expect_false(fs::dir_exists(fs::path(get_from_auxenv("aux_data_path"), "ppp.qs2")))
    expect_false(fs::file_exists(fs::path(get_from_auxenv("aux_data_path"), "ppp.qs2", "ppp.qs2")))

    dup_pfw <- aux_pfw_clean(make_valid_pfw_raw_fixture())[c(1L, 1L)]
    expect_error(pfw_validate_output(dup_pfw, detail = FALSE))
    expect_error(pip_aux_save(dup_pfw, id = "pfw", pk = pfw_pk, verbose = FALSE))
    expect_false(fs::file_exists(fs::path(get_from_auxenv("aux_data_path"), "pfw.qs2")))
    expect_false(fs::dir_exists(fs::path(get_from_auxenv("aux_data_path"), "pfw.qs2")))
    expect_false(fs::file_exists(fs::path(get_from_auxenv("aux_data_path"), "pfw.qs2", "pfw.qs2")))
  })
})

test_that("L-aux (3): re-saving unchanged CPI/PPP/PFW (even with changed raw GitHub SHA) skips new version and returns retained version_id, while changing one CPI value creates a new version", {
  with_isolated_aux_stamp({
    alias_name <- get_from_auxenv("aux_alias")

    sha_state <- new.env(parent = emptyenv())
    sha_state$cpi_sha <- "sha_cpi_1"
    sha_state$ppp_sha <- "sha_ppp_1"
    sha_state$pfw_sha <- "sha_pfw_1"
    sha_state$cpi2017_first <- 1.2
    sha_state$add_v2_2017 <- FALSE

    testthat::local_mocked_bindings(
      load_from_gh = function(measure, ...) {
        switch(
          measure,
          cpi = make_valid_cpi_raw_fixture(
            sha = sha_state$cpi_sha,
            cpi2017_first = sha_state$cpi2017_first
          ),
          ppp = make_valid_ppp_raw_fixture(
            sha = sha_state$ppp_sha,
            add_v2_2017 = sha_state$add_v2_2017
          ),
          pfw = make_valid_pfw_raw_fixture(sha = sha_state$pfw_sha),
          cli::cli_abort("Unexpected measure {.val {measure}}")
        )
      },
      .package = "pipfun"
    )

    cpi_v1 <- aux_cpi_update(branch = "20260324_TEST", detail = FALSE, verbose = FALSE)
    ppp_v1 <- aux_ppp_update(branch = "20260324_TEST", detail = FALSE, verbose = FALSE)
    pfw_v1 <- aux_pfw_update(branch = "20260324_TEST", detail = FALSE, verbose = FALSE)

    expect_equal(nrow(stamp::st_versions("cpi.qs2", alias = alias_name)), 1L)
    expect_equal(nrow(stamp::st_versions("ppp.qs2", alias = alias_name)), 1L)
    expect_equal(nrow(stamp::st_versions("pfw.qs2", alias = alias_name)), 1L)

    # Simulate data-neutral raw source commits (changed GitHub SHAs, identical cleaned content)
    sha_state$cpi_sha <- "sha_cpi_2_data_neutral"
    sha_state$ppp_sha <- "sha_ppp_2_data_neutral"
    sha_state$pfw_sha <- "sha_pfw_2_data_neutral"

    cpi_skip <- aux_cpi_update(branch = "20260324_TEST", detail = FALSE, verbose = FALSE)
    ppp_skip <- aux_ppp_update(branch = "20260324_TEST", detail = FALSE, verbose = FALSE)
    pfw_skip <- aux_pfw_update(branch = "20260324_TEST", detail = FALSE, verbose = FALSE)

    expect_true(cpi_skip$skipped)
    expect_true(ppp_skip$skipped)
    expect_true(pfw_skip$skipped)
    expect_identical(cpi_skip$version_id, cpi_v1$version_id)
    expect_identical(ppp_skip$version_id, ppp_v1$version_id)
    expect_identical(pfw_skip$version_id, pfw_v1$version_id)

    expect_equal(nrow(stamp::st_versions("cpi.qs2", alias = alias_name)), 1L)
    expect_equal(nrow(stamp::st_versions("ppp.qs2", alias = alias_name)), 1L)
    expect_equal(nrow(stamp::st_versions("pfw.qs2", alias = alias_name)), 1L)

    # Change one CPI value -> creates a new version and new version_id
    sha_state$cpi_sha <- "sha_cpi_3_data_change"
    sha_state$cpi2017_first <- 1.25

    cpi_v2 <- aux_cpi_update(branch = "20260324_TEST", detail = FALSE, verbose = FALSE)
    expect_false(cpi_v2$skipped)
    expect_false(identical(cpi_v2$version_id, cpi_v1$version_id))
    expect_equal(nrow(stamp::st_versions("cpi.qs2", alias = alias_name)), 2L)

    # Verify version-pinned read-back of v1 vs v2
    cpi_read_v1 <- pipload::pip_read("cpi", version = cpi_v1$version_id, alias = alias_name, verbose = FALSE)
    cpi_read_v2 <- pipload::pip_read("cpi", version = cpi_v2$version_id, alias = alias_name, verbose = FALSE)
    expect_equal(cpi_read_v1[country_code == "AAA" & cpi_year == 2017L, cpi_value], 1.2)
    expect_equal(cpi_read_v2[country_code == "AAA" & cpi_year == 2017L, cpi_value], 1.25)

    # Re-saving unchanged CPI after v2 exists skips and returns retained v2 version_id (not v1)
    sha_state$cpi_sha <- "sha_cpi_4_data_neutral_after_v2"
    cpi_skip_v2 <- aux_cpi_update(branch = "20260324_TEST", detail = FALSE, verbose = FALSE)
    expect_true(cpi_skip_v2$skipped)
    expect_identical(cpi_skip_v2$version_id, cpi_v2$version_id)
    expect_equal(nrow(stamp::st_versions("cpi.qs2", alias = alias_name)), 2L)

    # Adding a PPP vintage creates a new PPP version and updates default flags deterministically
    sha_state$ppp_sha <- "sha_ppp_3_vintage_added"
    sha_state$add_v2_2017 <- TRUE
    ppp_v2 <- aux_ppp_update(branch = "20260324_TEST", detail = FALSE, verbose = FALSE)
    expect_false(ppp_v2$skipped)
    expect_false(identical(ppp_v2$version_id, ppp_v1$version_id))
    expect_equal(nrow(stamp::st_versions("ppp.qs2", alias = alias_name)), 2L)

    ppp_read_v2 <- pipload::pip_read("ppp", version = ppp_v2$version_id, alias = alias_name, verbose = FALSE)
    expect_identical(
      ppp_read_v2[country_code == "AAA" & ppp_year == 2017 & ppp_default == TRUE,
                  paste(release_version, adaptation_version, sep = "_")],
      "v2_v1"
    )
  })
})

}
