#' Load Raw Auxiliary data
#'
#' @param measure
#' @param owner
#' @param repo
#' @param branch
#' @param tag
#'
#' @return dataset
#' @keywords internal
load_raw_aux <- function(measure,
                         owner   = "PIP-Technical-Team",
                         repo    = paste("aux_", measure),
                         branch  = c("DEV","PROD","main"),
                         tag     = match.arg(branch)) {
  #   ____________________________________________________________________________
  #   on.exit                                                                 ####
  on.exit({

  })

  #   ____________________________________________________________________________
  #   Defenses                                                                ####
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
    glue("https://github.com/{owner}/{repo}/raw/{tag}/{measure}.csv")

  tryCatch(
    expr = {
      # Your code...
      df <- suppressMessages(readr::read_csv(path))
      setDT(df)
    },
    # end of expr section

    error = function(e) {
      branch <- match.arg(branch)
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
          msg     <- c("Could not load {.field {measure}} from Github repo:
                     {e$message}")
          cli::cli_abort(msg, class = "pipaux_error")

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
          msg     <- c("Could not load {.field {measure}} from Github repo:
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
