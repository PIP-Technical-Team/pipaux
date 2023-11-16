#' Update the measure along with it's dependencies automatically.
#'
#' @param measure character: measure to be updated, if NULL will update all of
#'   them
#' @inheritParams pip_pop_update
#' @export
auto_aux_update <- function(measure = NULL,
                            force   = FALSE,
                            from    = c("gh", "file", "api"),
                            maindir = gls$PIP_DATA_DIR,
                            owner   = getOption("pipfun.ghowner"),
                            branch  = c("DEV", "PROD", "main"),
                            tag     = match.arg(branch)
                            ) {

  branch    <- match.arg(branch)
  from      <- match.arg(from)
  file_path <- system.file("extdata",
                           "git_metadata.csv",
                           package = "pipaux")

  # This should be created as a yml file that is store in the same plaseas
  # git_metadata.csv
  dependencies <- list(ppp = "country_list",
                       pfw = character(),
                       gdp = c("weo", "maddison", "wdi", "country_list"),
                       wdi = character(),
                       weo = c("pop"),
                       pop = c("country_list", "pfw"),
                       countries = c("pfw", "country_list"),
                       metadata = "pfw",
                       gdm = c("country_list", "pfw"),
                       regions = c("country_list"),
                       maddison = character(),
                       country_list = character(),
                       pce = c("wdi", "country_list"),
                       cpi = "country_list",
                       missing_data = c("country_list", "pce", "gdp", "pop", "pfw")
  )

  # Get all repositories under PIP-Technical-Team
  all_repos <- gh::gh("GET /users/{username}/repos",
                      username = owner) |>
    vapply("[[", "", "name") |>
  #Keep only those repos that start with "aux_"
    grep("^aux_", x = _, value = TRUE)

  # For testing purpose we are using only two repos
  # all_repos <- c("aux_ppp", "aux_pfw")

  # get hashs
  hash <-
    purrr::map(all_repos,
               .f = ~{
                 gh::gh("GET /repos/{owner}/{repo}/commits/{branch}",
                        owner = owner,
                        repo  = .x,
                        branch = branch)
               }) |>
    purrr::map_chr(~.x[["sha"]])

  # Get the latest hash of the repo
  all_data <- dplyr::tibble(Repo = glue::glue("{owner}/{all_repos}"),
                            hash = hash,
                            branch = branch)
  if(file_path == "") {
    new_data <- all_data
  } else {
    old_data <- readr::read_csv(file_path,
                                show_col_types = FALSE) |>
      dplyr::filter(branch == branch) %>%
      dplyr::rename(hash_original = hash)

    old_data <- old_data |>
      dplyr::full_join(all_data, by = c("Repo"))

    new_data <- old_data %>%
      dplyr::filter(hash != hash_original |
                      is.na(hash_original) |
                      is.na(hash))
  }


  # Remove everything till the last underscore so
  # PIP-Technical-Team/aux_ppp changes to ppp
  aux_fns <- sub(".*_", "", new_data$Repo) |>
    # Keep only those whose dependencies we know
    intersect(names(dependencies))

  # For each auxiliary data to be updated
  cli::cli_progress_bar(
    total = NA,
    format = "Running {.fn {fn}} for {.fn {aux}}
    | {cli::pb_bar} {cli::pb_percent}"
  )
  for(aux in aux_fns) {
    # Find the corresponding functions to be run
    # Add pip_ suffix so that it becomes function name
    list_of_funcs <- paste0("pip_", return_value(aux))
    for(fn in list_of_funcs) {
      Sys.sleep(0.01)
      cli::cli_progress_update()
      # Run the pip_.* function
      match.fun(fn)(maindir = maindir, branch = branch) |>
        suppressMessages()
    }
  }
  cli::cli_progress_done()

  #Write the latest auxiliary file and corresponding hash to csv
  # Always save at the end.
  readr::write_csv(all_data, file_path)

}


return_value <- function(aux) {
  val <- dependencies[[aux]]
  if(length(val) > 0) {
    for(i in val) {
      val <- c(return_value(i), val)
    }
  }
  return(unique(c(val, aux)))
}
