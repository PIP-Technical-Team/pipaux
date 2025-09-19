#' Update Country LIst
#'
#' @param class_branch character: names of branch of GPID-WB/class repo. Default
#'   if master
pip_country_list_update <-
  function(class_branch = "master") {

  # Check arguments
  measure <- "country_list"
  #   ____________________________________________________________________________
  #   Read data from CLASS.dta file                                           ####


  dt <- pipfun::load_from_gh(
    measure  = measure,
    owner    = "GPID-WB",
    repo     = "Class",
    branch   = class_branch,
    filename = "OutputData/CLASS",
    ext      = "dta"
  ) |>
    setDT()


  # collapse table to unique identifiers by country, NOT by country/year
  rm_names <- grep("year|historical", names(dt), value = TRUE)

  dt <- dt[,
           # max year per country to get current classifications
           .SD[year_data == fmax(year_data)], by = code
           ][,
             # Remove year variables
             (rm_names) := NULL] |>
    funique()

  setnames(x = dt,
           old = c("code", "economy" ),
           new = c("country_code", "country_name") )


  dt[country_code == "SOM",
     country_name := "Federal Republic of Somalia"]

  # hardcode fixing of TWN's name
  dt[country_code == "TWN",
     country_name  := "Taiwan, China"]

  # Hard code fix of SAS to SAR
  dt[regionpcn_code == "SAS",
     regionpcn_code := "SAR"]




  ssa_old <- grep("regionssa", names(dt), value = TRUE)
  ssa_news <- gsub("regionssa", "africa_split", ssa_old)


  setnames(dt,
           old = ssa_old,
           new = ssa_news)

  # Fragile countries
  dt[,
    fcv := fifelse(fcv == "Yes", "Fragile", "Not-fragile")]


  # Convert empty strings to NA
  recode_char(dt, `^$` = NA_character_,  regex = TRUE, set = TRUE)

  # Create the World

  dt[, `:=`(
    world      = "World",
    world_code =  "WLD"
  )]



  ## Order columns alphabetically ------------
  varn <- names(dt)
  setcolorder(dt, sort(varn))
  setcolorder(dt, c("country_code", "country_name",
                    "region", "region_code"))


  ## order data to get the right unicode
  setorder(dt, country_code)

  priority <- c("CIV","STP","CUW","TUR")

  wp  <- which( dt$country_code %in% priority)
  wnp <- which(!dt$country_code %in% priority)

  new_order <- c(wp, wnp)

  dt[new_order]

}
