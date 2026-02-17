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
  if (all(is.na(rep_var))) {
    return(ori_var)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculations   ---------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # find obs where rep_var  is  NOT missing but ori_var is


  working_obs <- which(!is.na(rep_var))
  x <- ori_var[working_obs]
  y <- rep_var[working_obs]

  # if the numb of obs to replace is 0 or one the algorithm
  # does not work.
  if (length(x) %in% c(0, 1)) {

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

#' Save auxiliary file to Github Repo
#'
#' Sometimes we need to save auxiliary files to Github repo.
#' This function allows for this.
#'
#' @inheritParams pipfun::save_to_gh
#' @export
#' @return NULL
save_aux_to_gh <- function(df,
                       measure,
                       owner     = getOption("pipfun.ghowner"),
                       repo      = paste0("aux_", measure),
                       branch    = "DEV",
                       tag       = branch,
                       filename  = measure,
                       ext       = "csv",
                         ...
                         ) {

  pipfun::save_to_gh(df = df,
                     repo = repo,
                     owner = owner,
                     branch = branch,
                     #tag = tag,
                     filename = filename,
                     ext = ext,
                     ...)
}

#' Extract value from `.pipaux` environment
#'
#' @param key Value to be extracted from `.pipaux` environment
#'
#' @returns Value for the key or NULL if key is not found
#'
get_from_auxenv <- \(key) {
  rlang::env_get(.pipaux, key, default = NULL) # Returns NULL if key doesn't exist
}

#' Save data to auxiliary data path
#'
#' @param x Data to be saved
#' @param id Name of the file
#'
#' @returns Fully qualified name of the new file, invisibly
#'
pip_aux_save <- \(x,
                  id,
                  force = FALSE,
                  ...) {

  # alias to pass into pipload::pip_write
  alias <- get_from_auxenv("aux_alias")

  # Save to the aux_data_path using pipload::pip_write
  pipload::pip_write(
    x        = x,
    id = id,
    alias = alias,
    ...
  )

  invisible(TRUE)
}

read_dependencies <- function(gh_user, owner) {
  dependencies <- paste(gh_user,
                        owner,
                        "pipaux/metadata/Data/new_dependency.yml",
                        sep = "/") |>
    yaml::read_yaml()

  sapply(dependencies, \(x) if (length(x))
    strsplit(x, ",\\s+")[[1]]
    else
      character())
}


# data.table is generally careful to minimize the scope for namespace
# conflicts (i.e., functions with the same name as in other packages);
# a more conservative approach using @importFrom should be careful to
# import any needed data.table special symbols as well, e.g., if you
# run DT[ , .N, by='grp'] in your package, you'll need to add
# @importFrom data.table .N to prevent the NOTE from R CMD check.
# See ?data.table::`special-symbols` for the list of such symbols
# data.table defines; see the 'Importing data.table' vignette for more
# advice (vignette('datatable-importing', 'data.table')).
#
#' @import data.table
NULL

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL


#' Hash code using stamp's hashing logic
#'
#' Internal wrapper around stamp:::st_hash_code()
#'
#' @param x A function, expression, or character vector
#' @keywords internal
hash_code <- function(x) {
  stamp:::st_hash_code(x)
}

plot_dependencies <- function(dependencies_all) {
  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required.")
  }

  measures <- names(dependencies_all)

  # build edge list: measure -> what it depends on
  edges <- do.call(
    rbind,
    lapply(measures, function(x) {
      deps <- dependencies_all[[x]]
      if (length(deps) == 0) return(NULL)  # no dependencies, no edges
      cbind(from = x, to = deps)
    })
  )

  # create graph including all measures (even those with no dependencies)
  g <- igraph::graph_from_data_frame(edges, vertices = measures, directed = TRUE)

  # color nodes: independent = depends on nothing (purple), others = light blue
  indep <- sapply(dependencies_all, function(x) length(x) == 0)
  cols <- ifelse(indep,
                 grDevices::adjustcolor("#C6A0DC", alpha.f = 0.75),  # independent
                 grDevices::adjustcolor("#9EC9FF", alpha.f = 0.75))  # depends on something

  # layout (Sugiyama works well for dependencies)
  lay <- igraph::layout_with_sugiyama(g)$layout
  igraph::E(g)$curved <- 0.15

  plot(
    g,
    layout = lay,
    vertex.size = 26,
    vertex.color = cols,
    vertex.frame.color = "white",
    vertex.label.font = 2,
    vertex.label.cex = 0.85,
    vertex.label.color = "grey20",
    edge.arrow.size = 0.35,
    edge.color = "grey70",
    margin = 0.2
  )

  legend(
    "topleft",
    legend = c("independent (no deps)", "depends on others"),
    fill = grDevices::adjustcolor(c("#C6A0DC", "#9EC9FF"), alpha.f = 0.75),
    border = NA,
    bty = "n",
    cex = 0.9
  )

  invisible(g)
}

#' Initialize auxiliary data update log
#'
#' Creates a new log for tracking auxiliary data updates within a cascade.
#' If a log already exists in the current cascade, reuses the existing log
#' instead of creating a new one.
#'
#' @param overwrite logical: If `TRUE` (default), overwrites existing log file.
#'   If `FALSE`, appends to existing log.
#'
#' @return character: Name of the initialized log
#'
#' @keywords internal
init_aux_log <- function(overwrite = TRUE) {

  # If already inside a cascade, reuse existing log
  if (rlang::env_has(.piplogenv, "active_aux_log")) {
    return(.piplogenv$active_aux_log)
  }

  # Unique name per cascade
  log_name <- paste0(
    "pipaux_update_log_",
    format(Sys.time(), "%Y%m%d_%H%M%S")
  )

  pipfun::log_init(log_name, overwrite = overwrite)

  .piplogenv$active_aux_log <- log_name
  .piplogenv$last_aux_log   <- log_name

  log_name
}

#' Finalize auxiliary data update log
#'
#' Cleans up the active log reference from the logging environment.
#' Called at the end of an auxiliary data update cascade to release
#' the active log.
#'
#' @return NULL (invisibly)
#'
#' @keywords internal
finalize_aux_log <- function() {
  if (rlang::env_has(.piplogenv, "active_aux_log")) {
    rlang::env_unbind(.piplogenv, "active_aux_log")
  }
}

#' Retrieve the last auxiliary data update log
#'
#' Returns the log object from the most recent auxiliary data update cascade.
#'
#' @return A log object containing entries from the last update cascade
#'
#' @keywords internal
aux_log_last <- function() {
  pipfun::log_get(.piplogenv$last_aux_log)
}
