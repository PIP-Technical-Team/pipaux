
#--------- To delete. Just for testing purposes ---------

pip_dlw_load <- function() {
  dlwdir <- getOption("pipaux.dlwdir")

  # cpid <<- haven::read_dta(paste0(dlwdir, "Final_CPI_PPP_to_be_used.dta"))
  # pfwd <<- haven::read_dta(paste0(dlwdir, "Survey_price_framework.dta"))
  # pppd <<- haven::read_dta(paste0(dlwdir, "pppdata_allvintages.dta"))
}


#--------- Find latest dlw directory ---------

latest_dlw_dir <- function(dlwdir){
  dlw_dirs <- dir(getOption("pipaux.dlwdir"))
  dt <- data.table(orig = dlw_dirs)

  cnames <-
    c(
      "country_code",
      "year",
      "survey_acronym",
      "vermast",
      "M",
      "veralt",
      "A",
      "collection"
    )

  latest <-
    dt[,
       # Name sections of filename into variables
       (cnames) := tstrsplit(orig, "_",
                             fixed=TRUE)
    ][!is.na(vermast) & !is.na(veralt)
    ][,
      maxmast := vermast == max(vermast)
    ][maxmast == TRUE
    ][,
      maxalt := veralt == max(veralt)
    ][maxalt == TRUE
    ][,
      orig
    ]

  return(latest)
}

#--------- Last item in character vector separator ---------

last_item <- function(x, word = "and") {
  if (!(is.character(x))) {
    warning("`x` must be character. coercing to character")
    x <- as.character(x)
  }

  lx <- length(x)
  if (lx == 1) {
    y <- x
  }
  else if (lx == 2) {
    y <- paste(x[1], word, x[2])
  }
  else {
    y <- c(x[1:lx-1], paste(word, x[lx]))
    y <- paste(y, collapse = ", ")
  }
  return(y)
}






