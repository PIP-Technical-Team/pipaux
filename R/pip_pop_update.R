#' Update population data
#'
#' @param src character: Source for Population data. Default is `emi`. Alternative
#' option is `wdi`.
#' @param maindir character: Path to main directory.
#' @param force logical: If TRUE force update.
#'
#' @return
#' @export
#' @import data.table
#'
#' @examples
pip_pop_update <- function(force, src = c("emi", "wdi"), maindir = getOption("pipaux.maindir")) {

  src <- match.arg(src)

  codes <- c("SP.POP.TOTL", "SP.RUR.TOTL", "SP.URB.TOTL")
  if (src == "wdi") {
    pop <- purrr::map_df(codes, ~{
      df <- wbstats::wb_data(indicator = .x, lang = "en")
      colnames(df)[colnames(df) == .x] <- "pop"
      df$coverage <- .x
      return(df)
    })

    setDT(pop)

    # data level
    pop[,
        pop_data_level :=
          fcase(
            grepl("POP", coverage), 2,
            grepl("RUR", coverage), 0,
            grepl("URB", coverage), 1
          )
    ]
    setnames(pop,
             old = c("iso3c", "date"),
             new = c("country_code", "year"))

  } else if (src == "emi") {

    # Load main data file from Emi

    # Get latest version of file
    pop_dir <- "P:/01.PovcalNet/03.QA/03.Population/data/"
    pop_files <- list.files(pop_dir, pattern = "population_country")
    pop_latest <- pop_files %>%
      gsub("population_country_|.xlsx", "", .) %>%
      as.POSIXlt() %>%
      max() %>%
      as.character() %>%
      sprintf("population_country_%s.xlsx", .)
    pop_path <- paste0(pop_dir, pop_latest)

    # Load data
    pop_main <- suppressMessages(
      readxl::read_xlsx(pop_path, sheet = "Sheet1"))
    names(pop_main)[1:4] <- as.character(pop_main[2, 1:4])
    pop_main <- pop_main[-c(1:2), ]
    year_vars <- names(pop_main[, 6:ncol(pop_main)])
    pop_main$Series_Name <- NULL
    pop_main$Time_Name <- NULL

    # Reshape to long format
    pop_long <- pop_main %>%
      data.table::setDT() %>%
      data.table::melt(id.vars = c("Country", "Series"),
                       measure.vars = year_vars,
                       variable.name = "Year",
                       value.name = "Population")
    pop_long$Year <- as.character(pop_long$Year)
    pop_long$Population <- as.numeric(pop_long$Population)

    # Merge with special country data
    # Note for PSE, KWT and SXM, some years of population data are missing in Emi's
    # main file and hence in WDI. Here we are complementing the main file with an
    # additional file she shared to assure complete coverage. This file contains
    # historical data and will not need to be updated every year. Hence, here we are
    # just calling the version we received. Should we receive a new version,
    # the import line below should be updated to reflect the accurate file.

    # Load data
    pop_special_file <- "population_missing_2020-12-01.xlsx"
    pop_special_path <- paste0(pop_dir, pop_special_file)
    pop_special <- suppressMessages(readxl::read_xlsx(
      pop_special_path, sheet = "Long"))
    pop_special <- pop_special[c("Country", "Series", "Time", "Data")]
    names(pop_special)[3:4] <- c("Year", "Population")
    pop_special$Year <- sub("YR", "", pop_special$Year)

    # Merge datasets
    pop_merge <- rbind(pop_long, pop_special)
    pop_merge$Year <- as.numeric(pop_merge$Year)
    setDT(pop_merge)

    # Create data_level column
    pop_merge[,
        pop_data_level :=
          fcase(
            grepl("POP", Series), 2,
            grepl("RUR", Series), 0,
            grepl("URB", Series), 1
          )
    ]

    setnames(
      pop_merge,
      old = c("Country", "Year", "Population"),
      new = c("country_code", "year", "pop"))

    pop_merge$Series <- NULL
    pop <- pop_merge
    pop <- pop[year <= 2020]


  } else {
    msg <- paste("src `", src,"` is not a valid source.")
    rlang::abort(c(
      msg,
      i = "make sure you select `wdi` or `emi`"
    ),
    class = "pipaux_error"
    )
  }

  pop <- pop[, c("country_code", "year", "pop_data_level", "pop")
  ][,
    pop_domain := fifelse(pop_data_level == 2, 1, 2)
  ]

  # recode domain and data_level variables
  cols <- c("pop_domain", "pop_data_level")
  pop[,
      (cols) := lapply(.SD, as.character),
      .SDcols = cols
  ][,# recode domain
    pop_domain := fcase(
      pop_domain == "1", "national",
      pop_domain == "2", "urban/rural",
      pop_domain == "3", "subnational region"
    )
  ][   # Recode data_level only for those that are national or urban/rural
    pop_domain %in% c("national", "urban/rural"),
    pop_data_level := fcase(
      pop_data_level == "0", "rural",
      pop_data_level == "1", "urban",
      pop_data_level == "2", "national"
    )
  ]

  measure <- "pop"
  msrdir  <- paste0(maindir, "_aux/", measure, "/")

  pip_sign_save(x       = pop,
                measure = "pop",
                msrdir  = msrdir,
                force   = force)
}

