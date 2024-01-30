# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# project:       Create a relational data model for Auxiliary data
##               code to prepare `aux_data_model` dataset
# Author:        R.Andres Castaneda
# Dependencies:  The World Bank
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Creation Date:    2023-02-27
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                   Load Libraries   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(dm)
library(pipaux)
library(dplyr)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   Get all auxiliary data   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Initial parameters --------

gls     <- pipfun::pip_create_globals()
maindir <- gls$PIP_DATA_DIR
branch  <- "DEV"
owner   <- getOption("pipfun.ghowner")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## directories and files --------

aux_dirs <-
  fs::path(maindir, "_aux/", branch) |>
  fs::dir_ls(type = "dir") |>
  fs::path_file()

aux_data <- lapply(aux_dirs, \(.) {
  load_aux(., maindir, branch )
})
names(aux_data) <- aux_dirs


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## flat lists --------

there_is_list <- function(x) {
  y <- lapply(x, class) |>
    unlist( ) |>
    unique()
  "list" %in% y
}

while (there_is_list(aux_data)) {
  wlist <- purrr::map_lgl(aux_data, ~{!is.data.frame(.x)}) |>
    which() |>
    names()

  aux_data <- purrr::list_flatten(aux_data)

  # Create empty data frames to complete diagram
  ldf <- lapply(wlist, \(.){
    data.frame(country_code = NA,
               region_code = NA)
  })
  names(ldf) <- wlist
  aux_data <- append(aux_data, ldf)
}

to_drop <- which(grepl("cp_flat", names(aux_data)))
aux_data[to_drop] <- NULL


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Additional files not available in local server --------

### National accounts ----------
sna <- pipfun::load_from_gh(
  measure = "sna",
  owner   = owner,
  branch = branch
)

sna_fy <- pipfun::load_from_gh(
  measure = "sna",
  owner   = owner,
  branch = branch,
  filename = "sna_metadata"
)

### special cases of population --------

spop <- pipfun::load_from_gh(
  measure = "pop",
  filename = "spop",
  owner  = owner,
  branch = branch)  |>
  pipaux:::clean_names_from_wide() |>
  pipaux:::clean_from_wide()



### Inventory -----------


inv <- fst::read_fst(fs::path(maindir, "_inventory/inventory.fst"),
                     as.data.table = TRUE)

## Append all ----------
aux_data <- append(aux_data, list(sna          = sna,
                                  sna_metadata = sna_fy,
                                  spop         = spop,
                                  inventory    = inv))
names(aux_data) |>
  sort()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#    Primary Keys                  ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dm_aux <- as_dm(aux_data)

dm_aux_pk <-
  dm_aux |>
  ## PCE
  dm_add_pk(pce,
            c(country_code, year, pce_data_level),
            check = TRUE) |>
  ## Poverty lines
  # dm_add_pk(pl,
  #           c(ppp_year, poverty_line),
  #           check = TRUE) |>
  ## CPI
  # dm_add_pk(cpi,
  #           c(country_code, cpi_year, survey_acronym, cpi_data_level),
  #           check = TRUE) |>
  ## PPP
  # dm_add_pk(
  #   ppp,
  #   c(
  #     country_code,
  #     ppp_year,
  #     release_version,
  #     adaptation_version,
  #     ppp_data_level
  #   ),
  #   check = TRUE
  # ) |>
  ## PFW
  dm_add_pk(pfw,
            c(country_code, year, survey_acronym),
            check = TRUE) |>
  ## Population
  dm_add_pk(pop,
            c(country_code, year, pop_data_level),
            check = TRUE) |>
  ## Special cases of population
  dm_add_pk(spop,
            c(country_code, year, pop_data_level),
            check = TRUE) |>
  ## GDP
  dm_add_pk(gdp,
            c(country_code, year, gdp_data_level),
            check = TRUE) |>
  ## WEO
  dm_add_pk(weo,
            c(country_code, year),
            check = TRUE) |>
  ## Maddison
  dm_add_pk(maddison,
            c(country_code, year),
            check = TRUE) |>
  ## WDI
  dm_add_pk(wdi,
            c(country_code, year),
            check = TRUE) |>
  ## Special National Accounts data
  dm_add_pk(sna,
            c(countrycode, year, coverage),
            check = TRUE) |>
  ## Metadata of Special National Accounts data
  dm_add_pk(sna_metadata,
            Code,
            check = TRUE) |>
  ## Country List
  dm_add_pk(country_list,
            country_code,
            check = TRUE) |>
  ## This is the one that includes all the countries
  dm_add_pk(countries,
            country_code,
            check = TRUE)
dm_aux_pk

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Foreign Key             ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

aux_data_model <-
  dm_aux_pk |>
  # dm_add_fk(gdp, c(country_code, year), weo) |>
  ## GDP --------
  dm_add_fk(gdp, c("country_code", "year"), weo) |> # works quoting
  dm_add_fk(gdp, c(country_code, year), maddison) |>
  dm_add_fk(gdp, c(country_code, year), wdi)  |>
  dm_add_fk(gdp, c(country_code, year, gdp_data_level),
            ref_table = sna,
            ref_columns = c(countrycode, year, coverage))  |>
  dm_add_fk(gdp, c(country_code), sna_metadata) |>
  dm_add_fk(gdp, c(country_code), country_list) |>
  ## PCE -----------
  dm_add_fk(pce, c(country_code, year), wdi)  |>
  dm_add_fk(pce, c(country_code, year, pce_data_level), sna)  |>
  dm_add_fk(pce, c(country_code), sna_metadata) |>
  dm_add_fk(pce, c(country_code), country_list) |>
  ## WEO -----------
  dm_add_fk(weo, c(country_code, year), pop, c(country_code, year)) |>

  ## Population -----------
  dm_add_fk(pop, c(country_code, year), pfw, c(country_code, year)) |>
  dm_add_fk(pop, c(country_code, year, pop_data_level), spop) |>
  dm_add_fk(pop, c(country_code), country_list) |>
  ## Countries -----------
  dm_add_fk(countries, c(country_code), country_list) |>
  dm_add_fk(countries, c(country_code), pfw, c(country_code)) |>
  ## Regions -----------
  dm_add_fk(regions, c(region_code), country_list) |>
  ## CPI -----------
  dm_add_fk(cpi, c(country_code), country_list) |>
  ## PPP -----------
  dm_add_fk(ppp, c(country_code), country_list) |>
  ## censoring -----------
  dm_add_fk(censoring, c(country_code), censoring_countries, country_code) |>
  dm_add_fk(censoring, c(country_code), censoring_regions, region_code) |>
  ## Group Data Mean -----------
  dm_add_fk(gdm, c(country_code, survey_year),
            ref_table = pfw,
            ref_columns = c(country_code, survey_year)) |>
  dm_add_fk(gdm, c("country_code", "surveyid_year"),
            ref_table = inventory,
            ref_columns = c("country_code", "surveyid_year")) |>
  dm_add_fk(gdm, c(country_code), country_list) |>
  ## Metadata -----------
  dm_add_fk(metadata,
            c(country_code, reporting_year, welfare_type),
            pfw,
            c(country_code, reporting_year, survey_acronym)) |>
  ## Missing Data ---------
  dm_add_fk(missing_data, c(country_code, year), gdp, c(country_code, year)) |>
  dm_add_fk(missing_data, c(country_code, year), pce, c(country_code, year)) |>
  dm_add_fk(missing_data, c(country_code, year), pop, c(country_code, year)) |>
  dm_add_fk(missing_data, c(country_code, year), pfw, c(country_code, year)) |>
  dm_add_fk(missing_data, c(country_code), country_list) |>
  ## Country Profile ---------
  dm_add_fk(cp, c(country_code), cp_charts, country_code ) |>
  dm_add_fk(cp, c(country_code), cp_key_indicators, country_code) |>
  ### Charts ---------
  dm_add_fk(cp_charts,
          c(country_code),
          cp_charts_ineq_trend,
          country_code) |>
  dm_add_fk(cp_charts,
            c(country_code),
            cp_charts_ineq_bar,
            country_code) |>
  dm_add_fk(cp_charts,
            c(country_code),
            cp_charts_mpm,
            country_code) |>
  dm_add_fk(cp_charts,
            c(country_code),
            cp_charts_sp,
            country_code) |>
  ### Key Indicators ---------
  dm_add_fk(
    cp_key_indicators,
    c(country_code),
    cp_key_indicators_headcount_national,
    country_code
  ) |>
  dm_add_fk(cp_key_indicators,
            c(country_code),
            cp_key_indicators_mpm_headcount,
            country_code) |>
  dm_add_fk(cp_key_indicators,
            c(country_code),
            cp_key_indicators_reporting_pop,
            country_code) |>
  dm_add_fk(cp_key_indicators,
            c(country_code),
            cp_key_indicators_gni,
            country_code) |>
  dm_add_fk(cp_key_indicators,
            c(country_code),
            cp_key_indicators_gdp_growth,
            country_code) |>
  dm_add_fk(
    cp_key_indicators,
    c(country_code),
    cp_key_indicators_shared_prosperity,
    country_code)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     Draw                 ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aux_data_model |>
  dm::dm_select_tbl(!starts_with("cp_")) |>
  dm::dm_draw()

aux_data_model |>
  dm::dm_draw(view_type = "title_only")


aux_data_model |>
  dm::dm_select_tbl(!matches("^cp_")) |>
  dm::dm_draw()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# save   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


usethis::use_data(aux_data_model,
                  overwrite = TRUE,
                  internal = TRUE)
