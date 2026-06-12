#' Update the measure along with it's dependencies automatically.
#'
#' @param measure character: measure to be updated, if NULL will update all of
#'   them
#' @inheritParams pip_pop_update
#' @export
auto_aux_update <- function(
  measure = NULL,
  force = FALSE,
  from = c("gh", "file", "api"),
  maindir = gls$PIP_DATA_DIR,
  owner = getOption("pipfun.ghowner"),
  branch = c("DEV", "PROD", "main"),
  tag = match.arg(branch)
) {
  pipfun::check_pkg_active("pipaux")

  branch <- match.arg(branch)
  from <- match.arg(from)
  files_changed <- FALSE

  cli::cli_progress_step("Retrieving information from Github")

  isgls <- ls(sys.frame(), pattern = "^gls$") |>
    length() >
    0

  if (isFALSE(isgls)) {
    cli::cli_abort(
      "object {.var gls} is not available in Global env.
      Run {.code gls <- pipfun::pip_create_globals()} first",
      wrap = TRUE
    )
  }

  creds <- pipfun::get_github_creds()
  gh_user <- "https://raw.githubusercontent.com"
  org_data <- read_git_metadata(
    owner = owner,
    token = creds$password
  )

  dependencies <- read_dependencies(
    gh_user = gh_user,
    owner = owner,
    token = creds$password
  )
  # Get all repositories under PIP-Technical-Team
  all_repos <- gh::gh("GET /users/{username}/repos", username = owner) |>
    vapply("[[", "", "name") |>
    #Keep only those repos that start with "aux_"
    grep("^aux_", x = _, value = TRUE)

  if (!is.null(measure)) {
    all_repos <- all_repos[all_repos %in% glue::glue("aux_{measure}")]
  }
  # Get the latest commit SHA for each repo, skipping repos that do not have
  # the target branch (e.g. newly created repos or repos with only a main/PROD
  # branch).  gh::gh() throws a 422 in those cases; fetch_repo_sha() absorbs
  # the error and returns NA_character_ so one bad repo can't crash the whole
  # update (Bug 1 fix).
  hash_results <- purrr::map(
    all_repos,
    .f = \(repo) fetch_repo_sha(owner = owner, repo = repo, branch = branch)
  )

  has_sha <- !vapply(hash_results, is.na, logical(1))
  all_repos <- all_repos[has_sha]
  hash <- unlist(hash_results[has_sha])

  cli::cli_progress_step("Comparing dependencies")

  # Get the latest hash of the repo
  all_data <-
    data.table(
      Repo = glue::glue("{owner}/{all_repos}"),
      hash = hash,
      branch = branch
    )

  br <- branch
  old_data <- org_data |>
    fsubset(branch == br) |>
    frename(hash_original = hash)

  repos_in_all <- all_data[, Repo] |>
    sort() |>
    fs::path_file()

  repos_in_old <- old_data[, Repo] |>
    sort() |>
    fs::path_file()

  diff_text <- list(
    "Repos not available in git_metadata.csv",
    "Aux measures that do not have a corresponding repository"
  )
  diffs <- list(
    setdiff(repos_in_all, repos_in_old),
    setdiff(repos_in_old, repos_in_all)
  )
  ldiffs <- sapply(diffs, length)

  if (any(ldiffs != 0)) {
    wdiffs <- which(ldiffs != 0)
    for (i in wdiffs) {
      cli::cli_alert_danger("{diff_text[[i]]}: {.field {diffs[[i]]}}")
    }
    cli::cli_alert_info(
      "Both the numbers above should be equal or else some
                      debugging is required.",
      wrap = TRUE
    )
  }

  old_data <- old_data |>
    join(all_data, on = c("Repo", "branch"), how = "inner")

  new_data <- old_data |>
    fsubset(
      hash != hash_original |
        is.na(hash_original) |
        is.na(hash)
    )

  # Remove prefix to get repo name
  # PIP-Technical-Team/aux_ppp changes to ppp and PIP-Technical-Team/aux_missing_countries becomes missing_countries
  aux_fns <- sub(paste0(owner, "/aux_"), "", new_data$Repo) |>
    # Keep only those whose dependencies we know
    intersect(names(dependencies))

  # Also include derived measures (those with no raw-data repo, like
  # missing_data) whose dependencies overlap with the changed set.  Without
  # this, pip_missing_data() is never called even when pfw/gdp/pop change
  # because there is no matching aux_missing_data GitHub repo (Bug 2 fix).
  aux_fns <- expand_with_derived_measures(aux_fns, dependencies)

  # For each auxiliary data to be updated
  cli::cli_alert_info(
    "Updating data for {length(aux_fns)} file{?s}.
                      {.field {aux_fns}}"
  )

  for (aux in aux_fns) {
    # Find the corresponding functions to be run
    # Add pip_ suffix so that it becomes function name
    fn <- ""

    list_of_funcs <- paste0("pip_", c(dependencies[[aux]], aux))

    for (fn in list_of_funcs) {
      # cli::cli_progress_update()
      cli::cli_inform("updating {aux} -- dependency {fn}")
      aux_file <- sub("pip_", "", fn)

      before_hash <- read_signature_file(aux_file, maindir, branch)
      # Run the pip_.* function
      run_aux_update_with_retry(
        update_fn = match.fun(fn),
        fn_name = fn,
        maindir = maindir,
        branch = branch
      )
      after_hash <- read_signature_file(aux_file, maindir, branch)

      # Use !isTRUE(== ) rather than != so that NA values (returned when the
      # signature file does not yet exist) are treated as "hashes differ" and
      # trigger the update (Bug 3 fix).
      if (!isTRUE(before_hash == after_hash)) {
        files_changed <- TRUE

        # find rows of of org to be modified
        aux_row_org <- org_data$Repo |>
          fs::path_file() |>
          sub('aux_', '', x = _) %in%
          aux_file &
          org_data$branch == branch

        # find rows in new that will be copied to org
        aux_row_new <- new_data$Repo |>
          fs::path_file() |>
          sub('aux_', '', x = _) %in%
          aux_file &
          new_data$branch == branch

        # Derived measures (e.g. missing_data) have no aux_* repo and
        # therefore no row in new_data.  Skip the SHA back-fill for those;
        # only measures with an actual repo entry need updating.
        if (any(aux_row_new)) {
          org_data$hash[aux_row_org] <- new_data$hash[aux_row_new]
        }
      } # end of before_hash condition
    } # end of list_of_funcs loop
  } # end of aux_fns loop

  cli::cli_progress_step("Update SHAs and git_metadata")
  last_updated_time <-
    aux_file_last_updated(maindir, names(dependencies), branch)

  if (length(aux_fns) > 0 && files_changed) {
    update_git_metadata_with_retry(org_data = org_data, token = creds$password)
  }
  cli::cli_h2("File updated status.")
  knitr::kable(last_updated_time)
}


return_value <- function(aux, dependencies) {
  val <- dependencies[[aux]]
  if (length(val) > 0) {
    for (i in val) {
      val <- c(return_value(i, dependencies), val)
    }
  }
  return(unique(c(val, aux)))
}

#' Function to write dataframe to GitHub
#'
#' @param df A dataframe
#'
#' @return base64 encoded dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' convert_df_to_base64(mtcars)
#' }
convert_df_to_base64 <- function(df) {
  df |>
    write.table(quote = FALSE, row.names = FALSE, sep = ",") |>
    capture.output() |>
    paste(collapse = "\n") |>
    charToRaw() |>
    base64enc::base64encode()
}

aux_file_last_updated <- function(data_dir, aux_files, branch) {
  filenames <-
    glue::glue("{data_dir}/_aux/{branch}/{aux_files}/{aux_files}.qs")
  data <- sapply(filenames, function(x) {
    qs::qattributes(x)$datetime
  })
  data.frame(
    filename = basename(names(data)),
    time_last_update = as.POSIXct(data, format = "%Y%m%d%H%M%S"),
    row.names = NULL
  ) |>
    dplyr::arrange(desc(time_last_update))
}

metadata_cache_dir <- function() {
  # Cache remote metadata locally so auto_aux_update() can recover from
  # transient GitHub failures, including HTTP 429 rate limiting.
  cache_dir <- tools::R_user_dir("pipaux", which = "cache")
  fs::dir_create(cache_dir)
  cache_dir
}

metadata_cache_file <- function(filename, owner) {
  fs::path(metadata_cache_dir(), glue::glue("{owner}_{filename}"))
}

packaged_metadata_file <- function(filename) {
  installed_file <- system.file("extdata", filename, package = "pipaux")

  if (nzchar(installed_file) && fs::file_exists(installed_file)) {
    return(installed_file)
  }

  dev_file <- fs::path("inst", "extdata", filename)

  if (fs::file_exists(dev_file)) {
    return(dev_file)
  }

  NA_character_
}

download_metadata_text <- function(
  owner,
  path,
  token = NULL,
  ref = "metadata"
) {
  # Retrieve files from the metadata branch through the GitHub contents API.
  # This supports authenticated requests and avoids depending on unauthenticated
  # raw URLs, which are more prone to rate limiting.
  response <- gh::gh(
    "GET /repos/{owner}/{repo}/contents/{path}",
    owner = owner,
    repo = "pipaux",
    path = path,
    .params = list(ref = ref),
    .token = token
  )

  response$content |>
    gsub(pattern = "\\n", replacement = "", x = _) |>
    base64enc::base64decode() |>
    rawToChar()
}

write_metadata_cache <- function(text, cache_file) {
  fs::dir_create(fs::path_dir(cache_file))
  writeLines(text, cache_file, useBytes = TRUE)
  invisible(cache_file)
}

read_metadata_cache <- function(cache_file) {
  if (!fs::file_exists(cache_file)) {
    return(NULL)
  }

  readr::read_file(cache_file)
}

parse_dependencies <- function(dependencies) {
  if (is.null(dependencies) || length(dependencies) == 0) {
    return(list())
  }

  lapply(dependencies, \(x) {
    if (is.null(x) || length(x) == 0 || all(is.na(x))) {
      return(character())
    }

    values <- as.character(x)

    if (length(values) == 1) {
      values <- strsplit(values, ",\\s*")[[1]]
    }

    values <- trimws(values)
    values[nzchar(values)]
  })
}

parse_dependencies_text <- function(text) {
  text |>
    yaml::yaml.load() |>
    parse_dependencies()
}

parse_git_metadata_text <- function(text) {
  text |>
    I() |>
    readr::read_csv(show_col_types = FALSE) |>
    setDT()
}

read_git_metadata <- function(owner, token = NULL) {
  # Resolution order:
  #   1. live file from GitHub
  #   2. last successful local cache
  #   3. packaged copy in inst/extdata
  # Git metadata is required for auto_aux_update(), so this helper aborts only
  # if all three sources are unavailable.
  cache_file <- metadata_cache_file("git_metadata.csv", owner)
  packaged_file <- packaged_metadata_file("git_metadata.csv")

  remote_text <- tryCatch(
    download_metadata_text(
      owner = owner,
      path = "Data/git_metadata.csv",
      token = token
    ),
    error = identity
  )

  if (!inherits(remote_text, "error")) {
    write_metadata_cache(remote_text, cache_file)
    return(parse_git_metadata_text(remote_text))
  }

  cli::cli_alert_warning(c(
    "Could not retrieve {.file Data/git_metadata.csv} from GitHub.",
    "i" = "Trying cached metadata instead.",
    "x" = conditionMessage(remote_text)
  ))

  cached_text <- tryCatch(read_metadata_cache(cache_file), error = identity)

  if (!inherits(cached_text, "error") && !is.null(cached_text)) {
    return(parse_git_metadata_text(cached_text))
  }

  if (inherits(cached_text, "error")) {
    cli::cli_alert_warning(c(
      "Could not read cached {.file git_metadata.csv}.",
      "i" = "Trying packaged metadata instead.",
      "x" = conditionMessage(cached_text)
    ))
  }

  if (!is.na(packaged_file)) {
    return(
      readr::read_csv(packaged_file, show_col_types = FALSE) |>
        setDT()
    )
  }

  cli::cli_abort(c(
    "Unable to load {.file git_metadata.csv}.",
    "x" = conditionMessage(remote_text),
    "i" = "No cached or packaged fallback was found."
  ))
}

read_dependencies <- function(gh_user, owner, token = NULL) {
  # Dependency metadata changes over time, so prefer a fresh download and store
  # the result in the local cache. If GitHub is temporarily unavailable, fall
  # back to the cache, then to any packaged copy. Unlike git metadata, missing
  # dependencies are non-fatal: auto_aux_update() can still proceed, although
  # it may skip dependency expansion for that run.
  cache_file <- metadata_cache_file("new_dependency.yml", owner)
  packaged_file <- packaged_metadata_file("new_dependency.yml")

  remote_text <- tryCatch(
    download_metadata_text(
      owner = owner,
      path = "Data/new_dependency.yml",
      token = token
    ),
    error = identity
  )

  if (!inherits(remote_text, "error")) {
    write_metadata_cache(remote_text, cache_file)
    return(parse_dependencies_text(remote_text))
  }

  cli::cli_alert_warning(c(
    "Could not retrieve {.file Data/new_dependency.yml} from GitHub.",
    "i" = "Trying cached dependency metadata instead.",
    "x" = conditionMessage(remote_text)
  ))

  cached_text <- tryCatch(read_metadata_cache(cache_file), error = identity)

  if (!inherits(cached_text, "error") && !is.null(cached_text)) {
    return(parse_dependencies_text(cached_text))
  }

  if (inherits(cached_text, "error")) {
    cli::cli_alert_warning(c(
      "Could not read cached {.file new_dependency.yml}.",
      "i" = "Trying packaged dependency metadata instead.",
      "x" = conditionMessage(cached_text)
    ))
  }

  if (!is.na(packaged_file)) {
    return(yaml::read_yaml(packaged_file) |> parse_dependencies())
  }

  cli::cli_alert_warning(c(
    "Proceeding without dependency metadata.",
    "i" = "No remote, cached, or packaged {.file new_dependency.yml} was available."
  ))

  list()
}

read_signature_file <- function(aux_file, maindir, branch) {
  # Construct the path to data signature aux file
  data_signature_path <-
    fs::path(
      maindir,
      "_aux",
      branch,
      aux_file,
      glue::glue("{aux_file}_datasignature.txt")
    )
  # Return NA when the file does not yet exist (first run for a measure).
  # The caller uses !isTRUE(before == after) so NA is treated as "differs"
  # and the update is triggered (Bug 3 fix).
  if (!fs::file_exists(data_signature_path)) {
    return(NA_character_)
  }
  readr::read_lines(data_signature_path)
}

# fetch_repo_sha -----------------------------------------------------------
# Wraps gh::gh() with error handling so that a repo that lacks the target
# branch (common for newly created aux repos) returns NA_character_ instead
# of aborting the entire purrr::map() loop (Bug 1 fix).
fetch_repo_sha <- function(owner, repo, branch) {
  tryCatch(
    gh::gh(
      "GET /repos/{owner}/{repo}/commits/{branch}",
      owner = owner,
      repo = repo,
      branch = branch
    )[["sha"]],
    error = function(e) {
      cli::cli_alert_warning(
        "Skipping {.field {repo}}: branch {.val {branch}} not found. \
{conditionMessage(e)}"
      )
      NA_character_
    }
  )
}

is_github_sha_conflict_error <- function(e) {
  message <- conditionMessage(e)

  grepl("GitHub API error \\(409\\)", message) &&
    grepl("does not match", message)
}

run_aux_update_with_retry <- function(
  update_fn,
  fn_name,
  maindir,
  branch,
  max_attempts = 3L,
  wait_seconds = 1
) {
  attempt <- 1L

  repeat {
    err <- NULL
    out <- tryCatch(
      suppressMessages(update_fn(maindir = maindir, branch = branch)),
      error = function(e) {
        err <<- e
        NULL
      }
    )

    if (is.null(err)) {
      return(invisible(out))
    }

    should_retry <- is_github_sha_conflict_error(err) && attempt < max_attempts

    if (!should_retry) {
      stop(err)
    }

    cli::cli_alert_warning(
      "SHA conflict while running {.field {fn_name}}. Retrying ({attempt}/{max_attempts})..."
    )
    Sys.sleep(wait_seconds * attempt)
    attempt <- attempt + 1L
  }
}

update_git_metadata_with_retry <- function(
  org_data,
  token,
  max_attempts = 3L,
  wait_seconds = 1
) {
  attempt <- 1L

  repeat {
    out <- gh::gh(
      "GET /repos/{owner}/{repo}/contents/{file_path}",
      owner = "PIP-Technical-Team",
      repo = "pipaux",
      file_path = "Data/git_metadata.csv",
      .params = list(ref = "metadata")
    )

    result <- tryCatch(
      gh::gh(
        "PUT /repos/{owner}/{repo}/contents/{path}",
        owner = "PIP-Technical-Team",
        repo = "pipaux",
        path = "Data/git_metadata.csv",
        .params = list(
          branch = "metadata",
          message = "updating csv file",
          sha = out$sha,
          content = convert_df_to_base64(org_data)
        ),
        .token = token
      ),
      error = identity
    )

    if (!inherits(result, "error")) {
      return(invisible(result))
    }

    should_retry <- is_github_sha_conflict_error(result) &&
      attempt < max_attempts

    if (!should_retry) {
      stop(result)
    }

    cli::cli_alert_warning(
      "SHA conflict while updating {.file Data/git_metadata.csv}. Retrying ({attempt}/{max_attempts})..."
    )
    Sys.sleep(wait_seconds * attempt)
    attempt <- attempt + 1L
  }
}

# expand_with_derived_measures --------------------------------------------
# Given a vector of changed measures (aux_fns) and the full dependency map,
# returns aux_fns extended with any measure whose dependency list overlaps
# with the changed set.  This ensures derived measures (e.g. missing_data)
# that have no corresponding aux_* repo are still updated when their inputs
# change (Bug 2 fix).
expand_with_derived_measures <- function(aux_fns, dependencies) {
  if (length(aux_fns) == 0 || length(dependencies) == 0) {
    return(aux_fns)
  }
  derived <- names(dependencies)[
    vapply(
      dependencies,
      function(deps) length(deps) > 0 && any(deps %in% aux_fns),
      logical(1)
    )
  ]
  # Exclude measures that are already in aux_fns to avoid duplicates
  union(aux_fns, setdiff(derived, aux_fns))
}
