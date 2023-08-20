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

 dependencies_path <- system.file("extdata",
                             "config.yml",
                             package = "pipaux")

 dependencies <- config::get(file = dependencies_path)
 dependencies <- sapply(dependencies, \(x) if(length(x)) strsplit(x, ",\\s+")[[1]] else character())

  # Get all repositories under PIP-Technical-Team
  all_repos <- gh::gh("GET /users/{username}/repos",
                      username = owner) |>
    vapply("[[", "", "name") |>
    #Keep only those repos that start with "aux_"
    grep("^aux_", x = _, value = TRUE)

  if(!is.null(measure)) {
    all_repos <- all_repos[all_repos %in% glue::glue("aux_{measure}")]
  }
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
    org_data <- readr::read_csv(file_path,
                                show_col_types = FALSE)
    old_data <- org_data %>%
      dplyr::filter(.data$branch == branch) %>%
      dplyr::rename(hash_original = hash)

    old_data <- old_data %>%
      dplyr::full_join(all_data, by = c("Repo", "branch"))

    new_data <- old_data %>%
      dplyr::filter(.data$hash != .data$hash_original |
                      is.na(.data$hash_original) |
                      is.na(.data$hash))

    all_data <- dplyr::rows_update(org_data, all_data, by = c("Repo", "branch"))
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
    list_of_funcs <- paste0("pip_", return_value(aux, dependencies))
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


return_value <- function(aux, dependencies) {
  val <- dependencies[[aux]]
  if(length(val) > 0) {
    for(i in val) {
      val <- c(return_value(i, dependencies), val)
    }
  }
  return(unique(c(val, aux)))
}
