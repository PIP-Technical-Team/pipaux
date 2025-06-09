#' Load any auxiliary data
#'
#' @inheritParams aux_pfw
#' @inheritParams pipfun::load_from_gh
#' @param apply_label logical: If TRUE, predefined labels will applied.
#' @param ppp_defaults logical: If TRUE, wider format ppp data will be returned
#'
#' @export
load_aux <- function(measure,
                     maindir      = getOption("pipaux.working_dir"),
                     apply_label  = TRUE,
                     ppp_defaults = TRUE,
                     branch       = NULL) {

  pipfun::get_wrk_release(verbose = FALSE)

  if (is.null(branch)) {
    release        <- wrk_release$release
    identity       <- wrk_release$identity
    branch         <- paste0(release, "_", identity)
  }

  if (branch == "main") {
    branch <- ""
  }

  msrdir <- fs::path(maindir, "aux_data/", branch, measure)

  file_paths <- fs::dir_ls(msrdir,
                           type = "file",
                           regexp = glue("/{measure}\\."))

  fun_read <- list(
    qs   = function(path) qs::qread(path),
    fst  = function(path) fst::read_fst(path),
    rds  = function(path) readr::read_rds(path)
  )

  fp  <- find_path(file_paths)
  ext <- fs::path_ext(fp)
  df <- fun_read[[ext]](fp)

  if (apply_label) {
    df <- aux_labels_pip(df, measure = measure)
  }

  if (inherits(df, "data.frame")) {
    setDT(df)
  }


  # PPP to wide
  if (measure == "ppp") {

    if (ppp_defaults) {
      # Keep default values only
      df <- df[ppp_default_by_year == TRUE]
    }

    # Q: do we need to keep the def and def by year vars after filtering?

    # Build version identifier
    df[, ppp_version := {
      x <- paste0("ppp_", ppp_year, "_", release_version, "_", adaptation_version)
      gsub("_v", "_0", x)
    }]

    # Collect version labels as attribute
    ppp_versions <- df[, unique(ppp_version)]

    # Reshape to wide
    df <- dcast(df,
                formula = country_code + reporting_level ~ ppp_version,
                value.var = "ppp")

    # Set attributes
    setattr(df, "aux_name", "ppp")
    setattr(df, "aux_key", c("country_code", "reporting_level"))
    setattr(df, "ppp_versions", ppp_versions)
  }

  # CPI to long

  # if (measure == "cpi") {
  #
  #   df <- melt(
  #     df,
  #     id.vars = setdiff(names(df), c("cpi2005", "cpi2011", "cpi2017", "cpi2021")),
  #     measure.vars = c("cpi2005", "cpi2011", "cpi2017", "cpi2021"),
  #     variable.name = "cpi_year",
  #     value.name = "cpi_value"
  #   )
  #
  #   # Convert 'cpi_year' from 'cpi2011' → numeric 2011
  #   #df[, cpi_year := as.integer(sub("cpi", "", cpi_year))]
  #
  #   setcolorder(df, c("country_code", "year", "cpi_year", "cpi_value"))
  #
  #   setattr(df, "aux_name", "cpi")
  #   setattr(df, "aux_key", c("country_code", "year", "cpi_year"))
  #
  #   return(df[])
  # }


  return(df)
}


#' Find path of file to be loaded depending on extension hierarchy
#'
#'
#' @param file_paths chracter: vector of file paths
#'
#' @return character vector of length 1 with preferred file path
find_path <- function(file_paths) {
  extensions <- fs::path_ext(file_paths)
  ext_order <- c("qs", "fst", "rds")

  f <- FALSE
  i <- 1
  while (f == FALSE) {

    ext <- ext_order[[i]]

    if(ext  %in% extensions) {
      p <- which(extensions == ext)
      f <- file_paths[[p]]
    } else {
      i <- i + 1
    }

  }

  if (f == FALSE) {
    msg     <- c(
      "File not found",
      "*" = "At least one of the following extension should be available: {.file {ext_order}}"
    )
    cli::cli_abort(msg,
                   class = "error_class")
  }

  return(f)

}

#' Load Raw Auxiliary data
#'
#' @description `r lifecycle::badge("superseded")`
#'
#' This function is deprecated because of the new, more flexible and general
#' function `pipfun::load_from_gh()`
#' @param measure character: measure to be loaded
#' @param owner character: Github repo owner. Default is
#'   `getOption("pipfun.ghowner")`
#' @param repo character: name of the repo
#' @param branch character: either "DEV" or "PROD". Refers to the branch that
#'   will be used to update either the development server or production.
#' @param tag character: specific release to be used in the update.
#' @param filename character: Name of file name without the ".csv" extension.
#'   Default is `measure`
#' @param ext character: Extension of `filename`. Default "csv"
#' @param ... parameters to be passed to the loading functions depending of the
#'   extension used
#'
#' @return dataset
#' @keywords internal
load_raw_aux <- function(measure,
                         owner     = getOption("pipfun.ghowner"),
                         repo      = paste0("aux_", measure),
                         branch    = c("DEV","PROD","main"),
                         tag       = match.arg(branch),
                         filename  = measure,
                         ext       = "csv",
                         ...) {

  lifecycle::deprecate_warn("0.1.0.9003",
                            "load_raw_aux()",
                            "pipfun::load_from_gh()")


  #   ____________________________________________________________________________
  #   on.exit                                                                 ####
  on.exit({

    if (exists("temp_file")) {
      if (fs::file_exists(temp_file)) {
        unlink(temp_file)
      }
    }
    # close(path)

  })

  #   ____________________________________________________________________________
  #   Defenses                                                                ####
  branch <- match.arg(branch)
  stopifnot(exprs = {

  })

  #   ____________________________________________________________________________
  #   Early returns                                                           ####
  if (FALSE) {
    return()
  }

  #   ____________________________________________________________________________
  #   Computations                                                            ####

  path <-
    glue("https://github.com/{owner}/{repo}/raw/{tag}/{filename}.{ext}")
  # path <- file(path)

  tryCatch(
    expr = {
      # load depending of the extension
      df <-  suppressMessages(  # suppress any loading message

        if (ext == "csv") {

          # readr::read_csv(path, ...)
          readr::read_csv(path, ...)

        } else if (ext  %in% c("xls", "xlsx")) {

          temp_file <- tempfile(fileext = ext)
          req <- httr::GET(path,
                           # write result to disk
                           httr::write_disk(path = temp_file))


          readxl::read_excel(path = temp_file, ...)

        } else if (ext == "dta") {

          haven::read_dta(path, ...)

        } else if (ext == "qs") {

          qs::qread(path, ...)

        } else if (ext == "fst") {

          fst::read_fst(path, ...)

        } else if (ext == "yaml") {

          yaml::read_yaml(path, ...)

        }

      )

      if (is.data.frame(df)) {
        setDT(df)
      }
    },
    # end of expr section

    error = function(e) {
      if (tag == branch) {

        ##  ............................................................................
        ##  Error in branches                                                       ####

        branches <- get_gh(owner, repo, what = "branches")

        if (!(branch  %in% branches)) {
          msg     <- c(
            "{.field branch} specified ({branch}) does not exist in repo
          {.file {owner}/{repo}}",
            "i" = "Select one among {.field {branches}}"
          )
          cli::cli_abort(msg, class = "pipaux_error")

        } else {
          msg     <- c("Problem loading {.file {filename}.{ext}} Correctly:
                     {e$message}")
          cli::cli_abort(msg, class = "pipaux_error",
                         wrap = TRUE)

        }

      } else {

        ##  ............................................................................
        ##  Error in tags                                                           ####

        tags     <- get_gh(owner, repo, what = "tags")

        if (!(tag  %in% tags)) {
          msg     <- c(
            "{.field tag} specified ({tag}) does not exist in repo
          {.file {owner}/{repo}}",
            "i" = "Select one among {.field {tags}}"
          )
          cli::cli_abort(msg, class = "pipaux_error")

        } else {
          msg     <- c("Could not load {.file {filename}.{ext}} from Github repo:
                     {e$message}")
          cli::cli_abort(msg, class = "pipaux_error")

        }
      }

    } # end of finally section

  ) # End of trycatch

  #   ____________________________________________________________________________
  #   Return                                                                  ####
  return(df)

}


