#' get_month_number
#' @param month character: Month in %B format (e.g. 'March')
#' @noRd
get_month_number <- function(month) {

  vapply(month, function(month){
    d <- sprintf("1-%s-1960", month)
    d <- as.Date(d, format = "%d-%B-%Y")
    m <-  substr((as.character(d)), 6, 7)
    m <- as.numeric(m)
    return(m)
  }, FUN.VALUE = numeric(1),
     USE.NAMES = FALSE)

}

#' days_in_month
#' @inheritParams get_month_number
#' @param year numeric: Year
#' @noRd
days_in_month <- function(month, year) {

  purrr::map2_dbl(month, year, .f = function(month, year){

    # Early return if missing
    if (is.na(month) || is.na(year)) return(NA_real_)

    # Create date
    d <- sprintf("1-%s-%s", month, year)
    d <- as.Date(d, format = "%d-%B-%Y")

    # Month number
    m <-  substr((as.character(d)), 6, 7)

    # Check for leap year
    leap <- 0
    if ((year %% 4 == 0 & year %% 100 != 0) | year %% 400 == 0)
      leap <- 1

    # Return the number of days in the month
    return(switch(m,
                  '01' = 31,
                  '02' = 28 + leap,
                  '03' = 31,
                  '04' = 30,
                  '05' = 31,
                  '06' = 30,
                  '07' = 31,
                  '08' = 31,
                  '09' = 30,
                  '10' = 31,
                  '11' = 30,
                  '12' = 31))
  })

}

#' Get number of decimals
#' @param x A numeric vector
#' @noRd
n_decimals <- function(x) {
  vapply(x, function(x) {
    if (abs(x - round(x)) > .Machine$double.eps^0.5) {
      nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]])
    } else {
      return(0)
    }
  }, FUN.VALUE = numeric(1))
}

#' Find latest dlw directory
#' @noRd
latest_dlw_dir <- function(dlwdir) {
  dlw_dirs <- dir(Sys.getenv("PIP_DLW_ROOT_DIR"))
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
    dt[
      ,
      # Name sections of filename into variables
      (cnames) := tstrsplit(orig, "_",
                            fixed = TRUE
      )
    ][
      !is.na(vermast) & !is.na(veralt)
    ][
      ,
      maxmast := vermast == max(vermast)
    ][
      maxmast == TRUE
    ][
      ,
      maxalt := veralt == max(veralt)
    ][
      maxalt == TRUE
    ][
      ,
      orig
    ]

  return(latest)
}

#' Last item in character vector separator
#' @noRd
last_item <- function(x, word = "and") {
  if (!(is.character(x))) {
    warning("`x` must be character. coercing to character")
    x <- as.character(x)
  }

  lx <- length(x)
  if (lx == 1) {
    y <- x
  } else if (lx == 2) {
    y <- paste(x[1], word, x[2])
  } else {
    y <- c(x[1:lx - 1], paste(word, x[lx]))
    y <- paste(y, collapse = ", ")
  }
  return(y)
}

#' Chain values
#'
#' Chain GDP or PCE values.
#'
#' @param dt data.table: A table.
#' @param base_var character: Name of base variable, e.g. 'wdi_gdp'.
#' @param replacement_var character: Name of replacement variable, e.g.
#'   'weo_gdp'.
#' @param new_name character: Name of replacement variable, e.g. 'weo_gdp'.
#' @param by character: A vector with columns to split `dt` by. Defaults to
#'   'country_code'.
#'
#' @keywords internal
chain_values <- function(dt, base_var, replacement_var, new_name, by = "country_code") {

  # allNA
  allNA <- function(x) all(is.na(x))

  # Add rowid by group
  dt$n <- data.table::rowidv(dt, cols = by)

  # Check if any groups have missing values for
  # all observations of base_var
  dt_na <- dt[,
              .SDcols = base_var, by = by,
              .(all_na = purrr::map_lgl(.SD, allNA))
  ]
  dt <- data.table::merge.data.table(dt, dt_na, by = by)

  # Create new_var (equal to base_var or replacement_var
  # if entire series is missing )
  dt$new_var <- data.table::fifelse(
    dt$all_na, dt[[replacement_var]], dt[[base_var]]
  )

  # Create lag and lead columns by group
  dt$rep_var <- dt[[replacement_var]]
  dt[,
     `:=`(
       rep_var_lag = shift(rep_var),
       rep_var_lead = shift(rep_var, type = "lead")
     ),
     by = by
  ]

  # Create linking factors (growth values)
  dt[,
     `:=`(
       # linking factors back
       fwd = (!is.na(rep_var) &
                !is.na(rep_var_lag) &
                n != 1) * (rep_var / rep_var_lag),
       # linking factors forward
       bck = (!is.na(rep_var) &
                !is.na(rep_var_lead) &
                n != .N) * (rep_var / rep_var_lead)
     ),
     by = by
  ]

  # Chain forwards
  # dt[, new_var := chain_forwards(.SD), by = by]
  setorderv(dt, c(by, "year"))
  dt[, new_var :=
       fifelse(is.na(new_var) & !is.na(rep_var) & !is.na(fwd),
               shift(new_var) * fwd,
               new_var
               ),
     by = by]

  # Chain backwards
  # dt[, new_var := chain_backwards(.SD), by = by]
  dt[, new_var :=
       fifelse(is.na(new_var) & !is.na(rep_var) & !is.na(bck),
               shift(new_var, type = "lead") * bck,
               new_var
       ),
     by = by]

  # Set new name
  setnames(dt, "new_var", new_name)

  # Remove temporary columns
  dt <- dt[, !c(
    "fwd", "bck", "rep_var", "rep_var_lag",
    "rep_var_lead", "all_na", "n"
  )]

  return(dt)
}

#' chain_forwards
#' @param dt data.table: A country-level data.table.
#' @noRd
chain_forwards <- function(dt) {
  n <- nrow(dt)
  for (i in seq(2, n)) {
    dt$new_var[i] <-
      data.table::fifelse( #
        is.na(dt$new_var[i]) & !is.na(dt$rep_var[i]) & !is.na(dt$fwd[i]),
        dt$new_var[i - 1] * dt$fwd[i],
        dt$new_var[i]
      )
  }
  return(dt$new_var)
}

#' chain_backwards
#' @param dt data.table: A country-level data.table.
#' @noRd
chain_backwards <- function(dt) {
  n <- nrow(dt)
  data.table::setorder(dt, -year)
  for (i in seq(2, n)) {
    dt$new_var[i] <-
      data.table::fifelse(
        is.na(dt$new_var[i]) & !is.na(dt$rep_var[i]) & !is.na(dt$bck[i]),
        dt$new_var[i - 1] * dt$bck[i],
        dt$new_var[i]
      )
  }
  data.table::setorder(dt, year)
  return(dt$new_var)
}

#' chain forward and then backward
#'
#' @param ori_var numeric: orignal variablke
#' @param rep_var numeric: replacement variable
#'
#' @keywords internal
chain <- function(ori_var,
                  rep_var) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Defensive setup   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Defenses --------
    stopifnot( exprs = {
        is.numeric(ori_var)
        is.numeric(rep_var)
      }
    )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Early Return --------

  # If no missing values, return original vector
  if (!anyNA(ori_var)) {
    return(ori_var)
  }
  # if all missing values, return replacement vector
  if (all(is.na(ori_var))) {
    return(rep_var)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # find obs where rep_var and fwd are NOT missing but ori_var is

  working_obs <- which(!is.na(rep_var))
  x <- ori_var[working_obs]
  y <- rep_var[working_obs]

  if (length(x) == 0) {
    return(ori_var)
  }

  while (any(is.na(x))) {

    ns   <-  which(is.na(x)) # index of NA obs
    dns  <- c(0, diff(ns))   # Difference between indexes of NA
    jns  <- which(dns > 1)   # those whose diff is greater than 1

    # IF there are NO differences greater than 1, ti means that all missing
    # values come one after the other. In that case, we get the last index of
    # missing values. If there is differences greater than one, it mees that
    # there is actual data between NAs, which will be used for calculations.
    if (length(jns) == 0) {

      # We are in a scenratio where there is only one series of missing values.
      # If there were more than one, we will be on `max(ns[jns])` below. In this
      # scenario, we don't know whether the series of missing is at the end of
      # at the beginning. So, if the higher index is the same as the length of
      # the vector, we start from the start. Otherwise, we start from the end.
      if (max(ns) == length(x)) {
        i <- min(ns)
      } else {
        i <-  max(ns)          # get the last obs with NA
      }
    } else {
      i    <- max(ns[jns])     # get the the one with greater diff
    }

    if (i > 1 &&
        !is.na(x[i - 1]) &&
        !is.na(y[i - 1])) {
      # chain forward
      x[i] <- x[i - 1] * (y[i] / y[i - 1])

    } else if (i < length(x)  &&
               !is.na(x[i + 1]) &&
               !is.na(y[i + 1])) {
      # chain backwards
      x[i] <- x[i + 1] * (y[i] / y[i + 1])
    }

  } # end of while

  ori_var[working_obs] <- x
  return(ori_var)
}

chain_val <- compiler::cmpfun(chain)


#' Get tags from specific Github repo
#'
#' @param owner character: Github username that owns the repo
#' @param repo character: Github repository name
#' @param what character: either "tags" or "branches"
#'
#' @return character vector with tags
get_gh <- function(owner,
                   repo,
                   what = c("tags", "branches")) {

  # on.exit ------------
  on.exit({

  })

  # Defenses -----------
  what <- match.arg(what)
  stopifnot( exprs = {

    }
  )

  # Early returns ------
  if (FALSE) {
    return()
  }

  # Computations -------

  rs <-
    gh::gh("/repos/{owner}/{repo}/{what}",
           owner = owner,
           repo = repo,
           what = what,
           .limit = Inf)  |>
    purrr::map_chr("name")

  if (what == "tags") {
    rs <- sort(rs, decreasing = TRUE)
  }


  # Return -------------
  return(rs)

}
