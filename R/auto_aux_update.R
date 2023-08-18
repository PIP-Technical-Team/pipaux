#' Update the measure along with it's dependencies automatically.
#'
#' @param measure character: measure to be updated, if NULL will update all of them
#' @inheritParams pip_pop_update
#' @param API_repo_data json output from Github (temporary argument)
#' @export
#'
auto_aux_update <- function(measure = NULL,
                            force   = FALSE,
                            from    = c("gh", "file", "api"),
                            maindir = gls$PIP_DATA_DIR,
                            owner   = getOption("pipfun.ghowner"),
                            branch  = c("DEV", "PROD", "main"),
                            tag     = match.arg(branch),
                            API_repo_data) {

  branch <- match.arg(branch)
  from <- match.arg(from)
  file_path <- system.file("extdata", "git_metadata.csv", package = "pipaux")
  #browser()
  # Get all repositories under PIP-Technical-Team
  # all_repos <- jsonlite::fromJSON("https://api.github.com/users/PIP-Technical-Team/repos")
  # #Keep only those repos that start with "aux_"
  # aux_repos <- all_repos %>%
  #   dplyr::filter(grepl("^aux_", name)) %>%
  #   dplyr::pull(full_name)

  # For testing purpose we are using only two repos
  # aux_repos <- c("PIP-Technical-Team/aux_ppp", "PIP-Technical-Team/aux_pfw")
  # Create URL with test branch
  #API_url <- glue::glue("https://api.github.com/repos/{aux_repos}/git/trees/{branch}?recursive=1")
  # Read the data
  #API_repo_data <- lapply(API_url, jsonlite::fromJSON)
  # Get the latest hash of the repo
  all_data <- dplyr::tibble(Repo = aux_repos, hash = purrr::map_chr(API_repo_data, `[[`, "sha"), branch = branch)
  if(file_path == "") {
    new_data <- all_data
  } else {
    old_data <- readr::read_csv(file_path, show_col_types = FALSE) %>%
      dplyr::filter(branch == branch) %>%
      dplyr::rename(hash_original = hash)

    old_data <- old_data %>% dplyr::full_join(all_data, by = c("Repo"))
    new_data <- old_data %>% dplyr::filter(hash != hash_original | is.na(hash_original) | is.na(hash))
  }
  #Write the latest auxiliary file and corresponding hash to csv
  readr::write_csv(all_data, file_path)

  # Remove everything till the last underscore so
  # PIP-Technical-Team/aux_ppp changes to ppp
  aux_fns <- sub(".*_", "", new_data$Repo)
  # Keep only those whose dependencies we know
  aux_fns <- intersect(aux_fns, names(dependencies))
  # Drop pip_gdp function because it gets stuck
  #aux_fns <- setdiff(aux_fns, "gdp")
  # For pip_cpi -> pip_cpi_clean
  options(pipaux.cpivar = "cpi2017")
  # For each auxiliary data to be updated
  for(aux in aux_fns) {
    # Find the corresponding functions to be run
    # Add pip_ suffix so that it becomes function name
    list_of_funcs <- paste0("pip_", return_value(aux))
    for(fn in list_of_funcs) {
      message(glue("Running function {fn} for aux function {aux}"))
      # Run the pip_.* function
      match.fun(fn)(maindir = maindir, branch = branch)
    }
  }

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
