# load data using `pip_dictionary()` from DEV branch
dictionary_data <- pip_dictionary(action = "load",
                   branch = "DEV")
var_list_unq <- c("region_name", "region_code", "year", "country_name",
                  "country_code", "reporting_level", "survey_acronym",
                  "survey_coverage", "welfare_time", "welfare_type",
                  "survey_time", "survey_comparability", "comparable_spell",
                  "poverty_line", "headcount", "poverty_gap",
                  "poverty_severity", "watts", "mean", "median", "mld",
                  "gini", "polarization", "decile1", "decile2", "decile3",
                  "decile4", "decile5", "decile6", "decile7", "decile8",
                  "decile9", "decile10", "cpi", "ppp", "population", "gdp",
                  "hfce", "is_interpolated", "distribution_type",
                  "pop_in_poverty")

test_that("Data dictionary variable list", {
  # skip_if_offline()
  expect_identical(dictionary_data[,variable], var_list_unq)
})
