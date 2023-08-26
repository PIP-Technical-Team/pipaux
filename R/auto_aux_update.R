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

  org_data <- readr::read_csv("https://raw.githubusercontent.com/PIP-Technical-Team/pipaux/metadata/Data/git_metadata.csv", show_col_types = FALSE)

  dependencies <- yaml::read_yaml("https://raw.githubusercontent.com/PIP-Technical-Team/pipaux/metadata/Data/dependency.yml")

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



  # Remove everything till the last underscore so
  # PIP-Technical-Team/aux_ppp changes to ppp
  aux_fns <- sub(".*_", "", new_data$Repo) |>
    # Keep only those whose dependencies we know
    intersect(names(dependencies))

  # For each auxiliary data to be updated
  cli::cli_alert_info("Updating data for {length(aux_fns)} files.")
  for(aux in aux_fns) {
    # Find the corresponding functions to be run
    # Add pip_ suffix so that it becomes function name
    list_of_funcs <- paste0("pip_", return_value(aux, dependencies))
    for(fn in list_of_funcs) {
      cli::cli_alert_info("Running function {fn} for aux file {aux}.")
      # Run the pip_.* function
      match.fun(fn)(maindir = maindir, branch = branch) |>
        suppressMessages()
    }
  }
  #Write the latest auxiliary file and corresponding hash to csv
  # Always save at the end.
  # sha - hash object of current csv file in Data/git_metadata.csv
  # content - base64 of changed data
  out <- gh::gh("GET /repos/{owner}/{repo}/contents/{file_path}",
            owner = "PIP-Technical-Team", repo = "pipaux", file_path = "Data/git_metadata.csv",
            .params = list(ref = "metadata"))

  res <- gh::gh("PUT /repos/{owner}/{repo}/contents/{path}", owner = "PIP-Technical-Team",
     repo = "pipaux", path = "Data/git_metadata.csv",
     .params = list(branch = "metadata", message = "updating csv file",
                    sha = out$sha,content = convert_df_to_base64(all_data)),
      .token = Sys.getenv("GITHUB_PAT"))

  message("File updated succesfully!!")

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

convert_df_to_base64 <- function(df) {
  df |>
    write.table(quote = FALSE, row.names = FALSE, sep=",") |>
    capture.output() |>
    paste(collapse="\n") |>
    charToRaw() |>
    base64enc::base64encode()
}
