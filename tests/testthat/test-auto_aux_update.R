dependencies <- list(ppp = "country_list",
                     pfw = character(),
                     gdp = c("weo", "maddison", "wdi", "country_list"),
                     wdi = character(),
                     weo = c("pop"),
                     pop = c("country_list", "pfw"),
                     countries = c("pfw", "country_list"),
                     metadata = "pfw",
                     gdm = c("country_list", "pfw"),
                     regions = c("country_list"),
                     maddison = character(),
                     country_list = character(),
                     pce = c("wdi", "country_list"),
                     cpi = "country_list",
                     missing_data = c("country_list", "pce", "gdp", "pop", "pfw")
)

test_that("return_value works as expected", {
  expect_equal(return_value("pce", dependencies), c("country_list", "wdi", "pce"))
  expect_equal(return_value("wdi", dependencies), "wdi")
  expect_equal(return_value("gdp", dependencies), c("country_list", "wdi", "maddison", "pfw", "pop", "weo", "gdp"))
})
