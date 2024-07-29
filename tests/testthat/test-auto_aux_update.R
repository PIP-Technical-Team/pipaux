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


base64_value_mtcars <- "bXBnLGN5bCxkaXNwLGhwLGRyYXQsd3QscXNlYyx2cyxhbSxnZWFyLGNhcmIKMjEsNiwxNjAsMTEwLDMuOSwyLjYyLDE2LjQ2LDAsMSw0LDQKMjEsNiwxNjAsMTEwLDMuOSwyLjg3NSwxNy4wMiwwLDEsNCw0CjIyLjgsNCwxMDgsOTMsMy44NSwyLjMyLDE4LjYxLDEsMSw0LDEKMjEuNCw2LDI1OCwxMTAsMy4wOCwzLjIxNSwxOS40NCwxLDAsMywxCjE4LjcsOCwzNjAsMTc1LDMuMTUsMy40NCwxNy4wMiwwLDAsMywyCjE4LjEsNiwyMjUsMTA1LDIuNzYsMy40NiwyMC4yMiwxLDAsMywxCjE0LjMsOCwzNjAsMjQ1LDMuMjEsMy41NywxNS44NCwwLDAsMyw0CjI0LjQsNCwxNDYuNyw2MiwzLjY5LDMuMTksMjAsMSwwLDQsMgoyMi44LDQsMTQwLjgsOTUsMy45MiwzLjE1LDIyLjksMSwwLDQsMgoxOS4yLDYsMTY3LjYsMTIzLDMuOTIsMy40NCwxOC4zLDEsMCw0LDQKMTcuOCw2LDE2Ny42LDEyMywzLjkyLDMuNDQsMTguOSwxLDAsNCw0CjE2LjQsOCwyNzUuOCwxODAsMy4wNyw0LjA3LDE3LjQsMCwwLDMsMwoxNy4zLDgsMjc1LjgsMTgwLDMuMDcsMy43MywxNy42LDAsMCwzLDMKMTUuMiw4LDI3NS44LDE4MCwzLjA3LDMuNzgsMTgsMCwwLDMsMwoxMC40LDgsNDcyLDIwNSwyLjkzLDUuMjUsMTcuOTgsMCwwLDMsNAoxMC40LDgsNDYwLDIxNSwzLDUuNDI0LDE3LjgyLDAsMCwzLDQKMTQuNyw4LDQ0MCwyMzAsMy4yMyw1LjM0NSwxNy40MiwwLDAsMyw0CjMyLjQsNCw3OC43LDY2LDQuMDgsMi4yLDE5LjQ3LDEsMSw0LDEKMzAuNCw0LDc1LjcsNTIsNC45MywxLjYxNSwxOC41MiwxLDEsNCwyCjMzLjksNCw3MS4xLDY1LDQuMjIsMS44MzUsMTkuOSwxLDEsNCwxCjIxLjUsNCwxMjAuMSw5NywzLjcsMi40NjUsMjAuMDEsMSwwLDMsMQoxNS41LDgsMzE4LDE1MCwyLjc2LDMuNTIsMTYuODcsMCwwLDMsMgoxNS4yLDgsMzA0LDE1MCwzLjE1LDMuNDM1LDE3LjMsMCwwLDMsMgoxMy4zLDgsMzUwLDI0NSwzLjczLDMuODQsMTUuNDEsMCwwLDMsNAoxOS4yLDgsNDAwLDE3NSwzLjA4LDMuODQ1LDE3LjA1LDAsMCwzLDIKMjcuMyw0LDc5LDY2LDQuMDgsMS45MzUsMTguOSwxLDEsNCwxCjI2LDQsMTIwLjMsOTEsNC40MywyLjE0LDE2LjcsMCwxLDUsMgozMC40LDQsOTUuMSwxMTMsMy43NywxLjUxMywxNi45LDEsMSw1LDIKMTUuOCw4LDM1MSwyNjQsNC4yMiwzLjE3LDE0LjUsMCwxLDUsNAoxOS43LDYsMTQ1LDE3NSwzLjYyLDIuNzcsMTUuNSwwLDEsNSw2CjE1LDgsMzAxLDMzNSwzLjU0LDMuNTcsMTQuNiwwLDEsNSw4CjIxLjQsNCwxMjEsMTA5LDQuMTEsMi43OCwxOC42LDEsMSw0LDI="

test_that("return_value works as expected", {
  expect_equal(return_value("pce", dependencies), c("country_list", "wdi", "pce"))
  expect_equal(return_value("wdi", dependencies), "wdi")
  expect_equal(return_value("gdp", dependencies), c("country_list", "wdi", "maddison", "pfw", "pop", "weo", "gdp"))
})

test_that("convert_df_to_base64 works as expected", {
  expect_equal(convert_df_to_base64(mtcars), base64_value_mtcars)
})
