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

  isgls <- ls(sys.frame(), pattern = "^gls$") |>
    length() > 0

  if (isFALSE(isgls)) {
    cli::cli_abort("object {.var gls} is not available in Globel env.
                  Run {.code gls <- pipfun::pip_create_globals()} first",
                   wrap = TRUE)
  }


  assertthat::assert_that(Sys.getenv("GITHUB_PAT") != "",
                          msg = "Enviroment variable `GITHUB_PAT` is empty. Please set it up using Sys.setenv(GITHUB_PAT = 'code')")
  gh_user   <- "https://raw.githubusercontent.com"
  org_data  <- paste(gh_user,
                     owner,
                     "pipaux/metadata/Data/git_metadata.csv",
                     sep = "/"
                     )  |>
    readr::read_csv(show_col_types = FALSE)


  dependencies <- read_dependencies(gh_user, owner)
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
    dplyr::inner_join(all_data, by = c("Repo", "branch"))

  cli::cli_alert_info("Number of rows from csv file : {nrow(old_data)}")
  cli::cli_alert_info("Number of rows from Github : {nrow(all_data)}")
  cli::cli_alert_info("Both the numbers above should be equal or else some debugging is required.")

  new_data <- old_data %>%
    dplyr::filter(.data$hash != .data$hash_original |
                    is.na(.data$hash_original) |
                    is.na(.data$hash))

  # all_data <- dplyr::rows_update(org_data, all_data, by = c("Repo", "branch"))



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
      aux_file <- sub("pip_", "", fn)
      cli::cli_alert_info("Running function {fn} for aux file {aux}.")
      before_hash <- read_signature_file(aux_file, maindir, branch)
      # Run the pip_.* function
      match.fun(fn)(maindir = maindir, branch = branch) |>
        suppressMessages()
      after_hash <- read_signature_file(aux_file, maindir, branch)
      if(before_hash != after_hash) {
        cli::cli_alert_info("Updating csv for {fn}")
        org_data$hash[org_data$branch == branch &
          fs::path_file(org_data$Repo) |> sub('aux_', '',x =  _) %in% aux_file] <-
          new_data$hash[fs::path_file(new_data$Repo) |> sub('aux_', '',x =  _) &
                          new_data$branch == branch]
      }
    }
  }
  last_updated_time <- aux_file_last_updated(maindir, names(dependencies), branch)
  if(length(aux_fns) > 0) {
    # Write the latest auxiliary file and corresponding hash to csv
    # Always save at the end.
    # sha - hash object of current csv file in Data/git_metadata.csv
    # content - base64 of changed data
    out <- gh::gh("GET /repos/{owner}/{repo}/contents/{file_path}",
              owner     = "PIP-Technical-Team",
              repo      = "pipaux",
              file_path = "Data/git_metadata.csv",
              .params   = list(ref = "metadata"))
      # There is no way to update only the lines which has changed using Github API
      # We need to update the entire file every time. Refer - https://stackoverflow.com/a/21315234/3962914
      res <- gh::gh("PUT /repos/{owner}/{repo}/contents/{path}",
                    owner   = "PIP-Technical-Team",
                    repo    = "pipaux",
                    path    = "Data/git_metadata.csv",
                    .params = list(branch  = "metadata",
                                   message = "updating csv file",
                                   sha     = out$sha,
                                   content = convert_df_to_base64(org_data)
                                   ),
                    .token = Sys.getenv("GITHUB_PAT")
                    )
  }
  cli::cli_h2("File updated status.")
  knitr::kable(last_updated_time)
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

aux_file_last_updated <- function(data_dir, aux_files, branch) {
  filenames <- glue::glue("{data_dir}/_aux/{branch}/{aux_files}/{aux_files}.qs")
  data <- sapply(filenames, function(x) qs::qattributes(x)$datetime)
  data.frame(filename = basename(names(data)),
             time_last_update = as.POSIXct(data, format = "%Y%m%d%H%M%S"), row.names = NULL) |>
    dplyr::arrange(desc(time_last_update))

}

read_dependencies <- function(gh_user, owner) {
  dependencies <- paste(gh_user,
                        owner,
                        "pipaux/metadata/Data/dependency.yml",
                        sep = "/"
  ) |>
    yaml::read_yaml()

  sapply(dependencies, \(x) if (length(x)) strsplit(x, ",\\s+")[[1]] else character())
}

read_signature_file <- function(aux_file, maindir, branch) {
  # Construct the path to data signature aux file
  data_signature_path <- fs::path(maindir, "_aux", branch, aux_file, glue::glue("{aux_file}_datasignature.txt"))
  signature_hash <- readr::read_lines(data_signature_path)
  return(signature_hash)
}
