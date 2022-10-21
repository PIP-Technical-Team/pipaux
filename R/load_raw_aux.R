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
          data.table::fread(path, ...)

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
