
## Initial parameters --------
branch  <- "DEV"
owner   <- getOption("pipfun.ghowner")
measure <- "pop"

test_that("pop_validate_raw() works identifying duplicate error", {

  pop_indicators <- c("SP.POP.TOTL", "SP.RUR.TOTL", "SP.URB.TOTL")
  pop   <- wbstats::wb_data(indicator = pop_indicators,
                            country = "all", # this is new
                            lang      = "en",
                            return_wide = FALSE) |>
    setDT()

  pop[, `:=` (indicator_id = fifelse(indicator_id == "SP.RUR.TOTL",
                                     "SP.URB.TOTL", indicator_id))]

  expect_error(pop_validate_raw(pop), "Duplicate error")

})

test_that("pop_validate_raw() works identifying type/ formating error", {

  pop_indicators <- c("SP.POP.TOTL", "SP.RUR.TOTL", "SP.URB.TOTL")
  pop   <- wbstats::wb_data(indicator = pop_indicators,
                            country = "all", # this is new
                            lang      = "en",
                            return_wide = FALSE) |>
    setDT()

  pop[, `:=` (date = as.character(date),
              value = as.character(value))]

  expect_error(pop_validate_raw(pop), "Type/ format error")

})

test_that("pop_validate_raw() works identifying invalid value", {

  pop_indicators <- c("SP.POP.TOTL", "SP.RUR.TOTL", "SP.URB.TOTL")
  pop   <- wbstats::wb_data(indicator = pop_indicators,
                            country = "all", # this is new
                            lang      = "en",
                            return_wide = FALSE) |>
    setDT()

  pop[, `:=` (indicator_id = fifelse(indicator_id == "SP.RUR.TOTL",
                                     "SP.RUR.totl", indicator_id))]

  expect_error(pop_validate_raw(pop), "Invalid value in `indicator_id`")

})

test_that("pop_validate_output() works identifying duplicate error", {

  pop <- load_aux(
    maindir = gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch
  )

  pop[, `:=` (pop_data_level = fifelse((pop_data_level == "rural" & country_code == "ABW"),
                                       "urban", pop_data_level))]

  expect_error(pop_validate_output(pop), "Duplicate error")

})

test_that("pop_validate_output() works identifying type/ formating error", {

  pop <- load_aux(
    maindir = gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch
  )

  pop[, `:=` (year = as.character(year),
              pop = as.character(pop))]

  expect_error(pop_validate_output(pop), "Type/ format error")

})

test_that("pop_validate_output() works identifying invalid value", {

  pop <- load_aux(
    maindir = gls$PIP_DATA_DIR,
    measure = measure,
    branch  = branch
  )

  pop[, pop_data_level := fifelse(pop_data_level == "national",
                                  "national1", pop_data_level)]

  expect_error(pop_validate_output(pop), "Invalid value in `pop_data_level`")

})
