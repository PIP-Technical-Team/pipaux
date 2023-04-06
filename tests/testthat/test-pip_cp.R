# load data using `pip_cp()` from DEV branch
cp_data <- pip_cp(action = "load",
                  branch = "DEV")

# key indicators
headcount_national <- cp_data$key_indicators$headcount_national

test_that("No duplicate records in Headcount (national) data", {

  expect_equal(any(duplicated(headcount_national,
                              by = c("country_code", "ppp_year",
                                     "reporting_year"))),
               FALSE)
})

test_that("Data types in headcount (national) data", {

  expect_true(is.character(headcount_national[["country_code"]]))
  expect_true(is.numeric(headcount_national[["ppp_year"]]))
  expect_true(is.numeric(headcount_national[["reporting_year"]]))

  # other variables
  expect_true(is.numeric(headcount_national[["headcount_national"]]))
})
# ------------------------------------------------------------ #
mpm_headcount <- cp_data$key_indicators$mpm_headcount

test_that("No duplicate records in mpm headcount data", {

  expect_equal(any(duplicated(mpm_headcount,
                              by = c("country_code", "ppp_year",
                                     "reporting_year"))),
               FALSE)
})

test_that("Data types in mpm headcount data", {

  expect_true(is.character(mpm_headcount[["country_code"]]))
  expect_true(is.numeric(mpm_headcount[["ppp_year"]]))
  expect_true(is.numeric(mpm_headcount[["reporting_year"]]))

  # other variables
  expect_true(is.numeric(mpm_headcount[["mpm_headcount"]]))
})
# ------------------------------------------------------------ #

reporting_pop <- cp_data$key_indicators$reporting_pop

test_that("No duplicate records in reporting population data", {

  expect_equal(any(duplicated(reporting_pop,
                              by = c("country_code", "reporting_year"))),
               FALSE)
})

test_that("Data types in reporting pop data", {

  expect_true(is.character(reporting_pop[["country_code"]]))
  expect_true(is.numeric(reporting_pop[["reporting_year"]]))

  # other variables
  expect_true(is.numeric(reporting_pop[["reporting_pop"]]))
})
# ------------------------------------------------------------ #

gni <- cp_data$key_indicators$gni

test_that("No duplicate records in gni data", {

  expect_equal(any(duplicated(gni,
                              by = c("country_code", "reporting_year"))),
               FALSE)
})

test_that("Data types in gni data", {

  expect_true(is.character(gni[["country_code"]]))
  expect_true(is.numeric(gni[["reporting_year"]]))

  # other variables
  expect_true(is.numeric(gni[["gni"]]))
  expect_true(is.logical(gni[["latest"]]))
})
# ------------------------------------------------------------ #

gdp_growth <- cp_data$key_indicators$gdp_growth

test_that("No duplicate records in gdp growth data", {

  expect_equal(any(duplicated(gdp_growth,
                              by = c("country_code", "reporting_year"))),
               FALSE)
})

test_that("Data types in gdp growth data", {

  expect_true(is.character(gdp_growth[["country_code"]]))
  expect_true(is.numeric(gdp_growth[["reporting_year"]]))

  # other variables
  expect_true(is.numeric(gdp_growth[["gdp_growth"]]))
  expect_true(is.logical(gdp_growth[["latest"]]))
})
# ------------------------------------------------------------ #

shared_prosperity <- cp_data$key_indicators$shared_prosperity

test_that("No duplicate records in shared prosperity data", {

  expect_equal(any(duplicated(shared_prosperity,
                              by = c("country_code", "ppp_year"))),
               FALSE)
})

test_that("Data types in shared prosperity data", {

  expect_true(is.character(shared_prosperity[["country_code"]]))
  expect_true(is.numeric(shared_prosperity[["ppp_year"]]))

  # other variables
  expect_true(is.character(shared_prosperity[["year_range"]]))
  expect_true(is.numeric(shared_prosperity[["share_below_40"]]))
  expect_true(is.numeric(shared_prosperity[["share_total"]]))
})
# ------------------------------------------------------------ #

# charts
ineq_trend <- cp_data$charts$ineq_trend

test_that("No duplicate records in inequality trend data", {

  expect_equal(any(duplicated(ineq_trend,
                              by = c("country_code", "reporting_year",
                                     "ppp_year"))),
               FALSE)
})

test_that("Data types in inequality trend data", {

  expect_true(is.character(ineq_trend[["country_code"]]))
  expect_true(is.numeric(ineq_trend[["reporting_year"]]))
  expect_true(is.numeric(ineq_trend[["ppp_year"]]))

  # other variables
  expect_true(is.character(ineq_trend[["survey_acronym"]]))
  expect_true(is.character(ineq_trend[["welfare_type"]]))
  expect_true(is.numeric(ineq_trend[["survey_comparability"]]))
  expect_true(is.character(ineq_trend[["comparable_spell"]]))
  expect_true(is.numeric(ineq_trend[["gini"]]))
  expect_true(is.numeric(ineq_trend[["theil"]]))
  expect_true(is.character(ineq_trend[["reporting_level"]]))
})
# ------------------------------------------------------------ #

ineq_bar <- cp_data$charts$ineq_bar

test_that("No duplicate records in inequality bar data", {

  expect_equal(any(duplicated(ineq_bar,
                              by = c("country_code", "reporting_year",
                                     "gender", "agegroup", "education",
                                     "distribution", "reporting_level",
                                     "ppp_year"))),
               FALSE)
})

test_that("Data types in inequality bar data", {

  expect_true(is.character(ineq_bar[["country_code"]]))
  expect_true(is.numeric(ineq_bar[["reporting_year"]]))
  expect_true(is.character(ineq_bar[["gender"]]))
  expect_true(is.character(ineq_bar[["agegroup"]]))
  expect_true(is.character(ineq_bar[["education"]]))
  expect_true(is.character(ineq_bar[["distribution"]]))
  expect_true(is.character(ineq_bar[["reporting_level"]]))
  expect_true(is.numeric(ineq_bar[["ppp_year"]]))

  # other variables
  expect_true(is.character(ineq_bar[["welfare_type"]]))
  expect_true(is.numeric(ineq_bar[["poverty_share_by_group"]]))
  expect_true(is.character(ineq_bar[["agegroup_label"]]))
  expect_true(is.character(ineq_bar[["education_label"]]))
  expect_true(is.character(ineq_bar[["gender_label"]]))
})
# ------------------------------------------------------------ #

mpm <- cp_data$charts$mpm

test_that("No duplicate records in mpm data", {

  expect_equal(any(duplicated(mpm,
                              by = c("country_code", "reporting_year",
                                     "ppp_year"))),
               FALSE)
})

test_that("Data types in mpm data", {

  expect_true(is.character(mpm[["country_code"]]))
  expect_true(is.numeric(mpm[["reporting_year"]]))
  expect_true(is.numeric(mpm[["ppp_year"]]))

  # other variables
  expect_true(is.character(mpm[["welfare_type"]]))
  expect_true(is.numeric(mpm[["mpm_education_attainment"]]))
  expect_true(is.numeric(mpm[["mpm_education_enrollment"]]))
  expect_true(is.numeric(mpm[["mpm_electricity"]]))
  expect_true(is.numeric(mpm[["mpm_sanitation"]]))
  expect_true(is.numeric(mpm[["mpm_water"]]))
  expect_true(is.numeric(mpm[["mpm_monetary"]]))
  expect_true(is.numeric(mpm[["mpm_headcount"]]))
})
# ------------------------------------------------------------ #

sp <- cp_data$charts$sp

test_that("No duplicate records in sp data", {

  expect_equal(any(duplicated(sp,
                              by = c("country_code", "year_range",
                                     "welfare_type" ,"distribution",
                                     "ppp_year", "reporting_level"))),
               FALSE)
})

test_that("Data types in shared prosperity data", {

  expect_true(is.character(sp[["country_code"]]))
  expect_true(is.character(sp[["year_range"]]))
  expect_true(is.character(sp[["welfare_type"]]))
  expect_true(is.character(sp[["distribution"]]))
  expect_true(is.character(sp[["reporting_level"]]))
  expect_true(is.numeric(sp[["ppp_year"]]))

  # other variables
  expect_true(is.numeric(sp[["shared_prosperity"]]))
})
# ------------------------------------------------------------ #

flat_cp <- cp_data$flat$flat_cp

test_that("No duplicate records in flat country profile data", {

  expect_equal(any(duplicated(flat_cp,
                              by = c("country_code", "reporting_year",
                                     "ppp_year"))),
               FALSE)
})

test_that("Data types in flat cp data", {

  expect_true(is.character(flat_cp[["country_code"]]))
  expect_true(is.numeric(flat_cp[["reporting_year"]]))
  expect_true(is.numeric(flat_cp[["ppp_year"]]))

  # other variables
  expect_true(is.numeric(flat_cp[["welfare_time"]]))
  expect_true(is.character(flat_cp[["survey_coverage"]]))
  expect_true(is.character(flat_cp[["is_interpolated"]]))
  expect_true(is.character(flat_cp[["survey_acronym"]]))
  expect_true(is.numeric(flat_cp[["survey_comparability"]]))
  expect_true(is.character(flat_cp[["comparable_spell"]]))
  expect_true(is.character(flat_cp[["welfare_type"]]))
  expect_true(is.numeric(flat_cp[["headcount_ipl"]]))
  expect_true(is.numeric(flat_cp[["headcount_lmicpl"]]))
  expect_true(is.numeric(flat_cp[["headcount_umicpl"]]))
  expect_true(is.numeric(flat_cp[["headcount_national"]]))
  expect_true(is.numeric(flat_cp[["headcount_national_footnote"]]))
  expect_true(is.numeric(flat_cp[["gini"]]))
  expect_true(is.numeric(flat_cp[["theil"]]))
  expect_true(is.numeric(flat_cp[["share_b40_female"]]))
  expect_true(is.numeric(flat_cp[["share_t60_female"]]))
  expect_true(is.numeric(flat_cp[["share_b40_male"]]))
  expect_true(is.numeric(flat_cp[["share_t60_male"]]))
  expect_true(is.numeric(flat_cp[["share_b40_rural"]]))
  expect_true(is.numeric(flat_cp[["share_t60_rural"]]))
  expect_true(is.numeric(flat_cp[["share_b40_urban"]]))
  expect_true(is.numeric(flat_cp[["share_t60_urban"]]))
  expect_true(is.numeric(flat_cp[["share_b40agecat_0_14"]]))
  expect_true(is.numeric(flat_cp[["share_t60agecat_0_14"]]))
  expect_true(is.numeric(flat_cp[["share_b40agecat_15_64"]]))
  expect_true(is.numeric(flat_cp[["share_t60agecat_15_64"]]))
  expect_true(is.numeric(flat_cp[["share_b40agecat_65p"]]))
  expect_true(is.numeric(flat_cp[["share_t60agecat_65p"]]))
  expect_true(is.numeric(flat_cp[["share_b40edu_noedu"]]))
  expect_true(is.numeric(flat_cp[["share_t60edu_noedu"]]))
  expect_true(is.numeric(flat_cp[["share_b40edu_pri"]]))
  expect_true(is.numeric(flat_cp[["share_t60edu_pri"]]))
  expect_true(is.numeric(flat_cp[["share_b40edu_sec"]]))
  expect_true(is.numeric(flat_cp[["share_t60edu_sec"]]))
  expect_true(is.numeric(flat_cp[["share_b40edu_ter"]]))
  expect_true(is.numeric(flat_cp[["share_t60edu_ter"]]))
  expect_true(is.numeric(flat_cp[["mpm_education_attainment"]]))
  expect_true(is.numeric(flat_cp[["mpm_education_enrollment"]]))
  expect_true(is.numeric(flat_cp[["mpm_electricity"]]))
  expect_true(is.numeric(flat_cp[["mpm_sanitation"]]))
  expect_true(is.numeric(flat_cp[["mpm_water"]]))
  expect_true(is.numeric(flat_cp[["mpm_monetary"]]))
  expect_true(is.numeric(flat_cp[["mpm_headcount"]]))
})
# ------------------------------------------------------------ #

flat_shp <- cp_data$flat$flat_shp

flap_shp_var_list <- names(flat_shp)
flap_shp_var_list_chr <- c("country_code", "welfare_type",
                           "is_interpolated", "xyzdmxyzcoverage")
flap_shp_var_list_num <- flap_shp_var_list[!(flap_shp_var_list %in% flap_shp_var_list_chr)]

test_that("No duplicate records in flat shared prosperity data", {

  expect_equal(any(duplicated(flat_shp,
                              by = c("country_code", "ppp_year", "welfare_type"))),
               FALSE)
})

test_that("Data types in flat shared prosparity data", {

  # character data type
  expect_true(sapply(flap_shp_var_list_chr,
                     function(i) is.character(flat_shp[[i]])))

  # numeric data type
  expect_true(sapply(flap_shp_var_list_num,
                     function(i) is.numeric(flat_shp[[i]])))
})
# ------------------------------------------------------------ #
