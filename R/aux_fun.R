#' General auxiliary function
#'
#' Update any auxiliary data
#'
#' This function updates any auxiliary data.
#' It ensures that dependencies are processed before handling the specified measure and checks whether updates
#' are needed for GitHub or the Y drive.
#'
#' @param measure character: Name of the measure to process
#' @param action character: Either "update" or "load". Default is "update"
#'   - If `"update"`, data will be updated on the system (GitHub and/or Y drive)
#'   (- If `"load"`, data will be loaded into memory.) TBD
#' @param repo character: Name of the GitHub repository containing the auxiliary data
#'   Defaults to `"aux_<measure>"` unless `measure` is `"income_groups"` or `"country_list"`, in which case it defaults to `"Class"`.
#' @param owner character: GitHub repository owner. Default is `getOption("pipfun.ghowner")`
#' @param maindir character: Main directory of the project. Default is `getOption("pipaux.working_dir")`
#' @param processed environment: Environment that keeps track of already processed measures to avoid redundant processing.
#'   Default is a new empty environment
#' @param force logical: If `TRUE`, forces an update even if the data appears up to date.
#'   Default is `FALSE`
#' @param tag character: Specifies the GitHub branch tag for versioning.
#'   Defaults to `release_branch`, which is determined from the working release
#' @param ... Additional arguments passed to the auxiliary function handling the measure
#'
#' @return This function does not return a value but performs the requested action (load/update).
#' @examples
#' \dontrun{
#'   # Update a specific measure (e.g., gdp data)
#'   aux_fun(measure = "gdp", action = "update")
#'
#'
#' }
#' @export
aux_fun <- function(measure,
                    action    = c("update", "load"),
                    repo      = paste0("aux_", measure),
                    owner     = getOption("pipfun.ghowner"),
                    maindir   = getOption("pipaux.working_dir"),
                    processed = new.env(parent = emptyenv()),
                    force     = FALSE,
                    tag       = NULL,
                    ...) {

  # Set arguments
  action         <- match.arg(action)

  # Get working release
  # Check if wrk_release exists
  if (!rlang::env_has(.GlobalEnv, "wrk_release")) {
    pipfun::get_wrk_release()
  }


  release        <- wrk_release$release
  identity       <- wrk_release$identity
  release_branch <- paste0(release, "_", identity)

  if (is.null(tag)) {
    tag <- release_branch
  }

  # Set repo to "Class" if measure is "income_groups" or "country_list"
  repo  <- if (measure %in% c("income_groups", "country_list")) "Class" else repo

  # If measure has already been processed, skip it
  if (rlang::env_has(processed, measure)) {
    cli::cli_alert_info("{measure} has already been processed, skipping dependencies...")

  } else {
    # Mark this measure as processed
    rlang::env_poke(processed, measure, TRUE)

    # Read all dependencies
    dependencies_all <- read_dependencies(
      gh_user = "https://raw.githubusercontent.com",
      owner   = "PIP-Technical-Team"
    )

    # Get dependencies for this measure; if none, default to an empty vector
    dependencies <- dependencies_all[[measure]]

    if (is.null(dependencies)) {
      dependencies <- character(0)
    }

    # Create progress bar
    if (length(dependencies) > 0) {
      cli::cli_progress_bar(
        format = "Processing {dep} ({.current}/{.total})",
        total  = length(dependencies),
        type   = "iterator"
      )
    }

    # Recursively process dependencies
    for (i in seq_along(dependencies)) {

      dep <- dependencies[i]

      cli::cli_progress_message("Processing {dep}...")

      tryCatch(
        {
          aux_fun(
            measure   = dep,
            action    = action,
            repo      = paste0("aux_", dep),
            owner     = owner,
            maindir   = maindir,
            processed = processed,
            force     = force,
            tag       = tag,
            ...
          )
        },
        error = function(e) {
          cli::cli_alert_danger("Error processing {dep}: {conditionMessage(e)}")
        }
      )
    } # end of dependencies loop
    cli::cli_progress_done()



  # Check update status for the current measure
  check_status <- check_status(measure = measure,
                               repo    = repo,
                               owner   = owner,
                               maindir = maindir)

  update_gh <- check_status$update_gh
  update_y  <- check_status$update_y

  if (!update_gh && !update_y) {
    cli::cli_alert_info("No action required. GitHub and Y drive already up to date")
    return(invisible(NULL))
  }

  # Update: GH first and then Y
  if (update_y) {

    # Update GitHub if necessary
    if (update_gh) {

      pipfun::sync_release_branch(
        owner      = owner,
        repo       = repo,
        ref_branch = "DEV",
        target_branch = release_branch
      )
    }

    # Retrieve and execute the function from the pipaux namespace
    function_name <- paste0("aux_", measure)

    if (!exists(function_name, envir = asNamespace("pipaux"))) {
      cli::cli_abort(paste0("Function '", function_name, "' does not exist in the 'pipaux' package."))
    }

    # check again where this is taken from !!
    func <- get(function_name,
                envir = asNamespace("pipaux"))

    # Build a list of all possible arguments to pass
    all_args <- c(
      list(
        action  = action,
        maindir = maindir,
        branch  = release_branch,
        force   = force,
        owner   = owner,
        tag     = tag,
        repo    = repo
      ))

    # Retrieve the formal arguments of the function
    formal_args <- names(formals(func))

    # Filter to include only matching arguments
    filtered_args <- all_args[names(all_args) %in% formal_args]

    # Call the function with the filtered arguments
    do.call(func, filtered_args)
  }

  } # close else

  invisible(NULL)
}



#' Check update status of a measure
#'
#' Determines whether a measure needs to be updated on GitHub and/or the Y drive.
#'
#' @param measure Character: Name of the measure to check.
#' @param repo Character: Repository name. Defaults to "aux_<measure>".
#' @param owner Character: GitHub repository owner. Defaults to `getOption("pipfun.ghowner")`.
#' @param maindir Character: Main directory path. Defaults to `getOption("pipaux.working_dir")`.
#' @param verbose Logical: If TRUE, prints messages about the status. Default is TRUE.
#'
#' @return A list with `update_gh` (logical) indicating if GitHub needs updating,
#'   and `update_y` (logical) indicating if the Y drive needs updating.
#' @keywords internal
check_status <- function(measure,
                         repo       = paste0("aux_", measure),
                         owner      = getOption("pipfun.ghowner"),
                         maindir    = getOption("pipaux.working_dir"),
                         verbose    = TRUE) {

  if (!rlang::env_has(.GlobalEnv, "wrk_release")) {
    pipfun::get_wrk_release()
  }

  release        <- wrk_release$release
  identity       <- wrk_release$identity
  release_branch <- paste0(release, "_", identity)

  if (verbose) {
    cli::cli_h1("Checking Status for {measure}")
  }

  update_gh      <- TRUE

  # Retrieve GitHub branches
  gh_branches <- tryCatch(
    pipfun::get_repo_branches(owner = owner, repo = repo),
    error = function(e) {
      if (verbose) cli::cli_alert_danger("GitHub repository not found or an error occurred: {e$message}")
      return(NULL)
    }
  )

  # Determine if GitHub needs an update
  if (is.null(gh_branches)) {

    update_gh <- FALSE  # Repo not found or error

  } else if (release_branch %in% gh_branches$release_branches) {

    release_up_to_date <- pipfun::compare_branch_content(
      owner    = owner,
      repo     = paste0("aux_", measure),
      branch1  = "DEV",
      branch2  = release_branch,
      verbose = FALSE
    )$same_content

    if (release_up_to_date) {
      if (verbose) cli::cli_alert_success(
        "GitHub branch {.strong {release_branch}} is up to date with DEV.")

    } else {
      if (verbose) cli::cli_alert_warning(
        "GitHub branch {.strong {release_branch}} is outdated. Update required."
        )
    }

    update_gh <- !release_up_to_date
  }

  if (update_gh) {

    update_y <- TRUE

    if (verbose) {
      cli::cli_h1("Summary")
      cli::cli_alert_info("Update GitHub: {.strong {update_gh}}")
      cli::cli_alert_info("Update Y drive: TRUE")

    }

    return(invisible(list(update_gh = update_gh,
                update_y  = update_y)))
  }

  # Check if Y drive file exists
  y_file_path <- fs::path(maindir, "aux_data", release_branch, measure, measure, ext = "qs")

  if (verbose) cli::cli_alert_info("Checking file: {y_file_path}")

  if (!fs::file_exists(y_file_path)) {

    cli::cli_alert_danger("File {y_file_path} does not exist.")

    update_y <- TRUE

    # if (verbose) {
    #   cli::cli_h1("Summary")
    #   cli::cli_alert_info("Update GitHub: {.strong {update_gh}}")
    #   cli::cli_alert_info("Update Y drive: {.strong {update_y}}")
    # }

    if (verbose) {
      summary_text <- c(
        "Summary",
        sprintf("Update GitHub: %s", cli::style_bold(update_gh)),
        sprintf("Update Y drive: %s", cli::style_bold(update_y))
      )
      cli::boxx(summary_text, padding = 1, border_style = "double", align = "left")
    }

    return(invisible(list(update_gh = update_gh,
                          update_y  = update_y)))
  }

  # Retrieve stored GitHub metadata from Y drive file
  gh <- qs::qattributes(y_file_path)$gh

  if (length(gh) > 0 && !is.list(gh[[1]])) gh <- list(gh_list = gh)

  # Function to get SHA from GitHub
  get_gh_sha <- function(gh_entry) {

    tryCatch(
      pipfun::get_file_info_from_gh(
        owner     = gh_entry$owner,
        repo      = gh_entry$repo,
        branch    = gh_entry$branch,
        file_path = gh_entry$file_path
      )$sha,
      error = function(e) {
        if (verbose) cli::cli_alert_danger("File not found or another error occurred: {e$message}")
        NULL
      }
    )

  }

  # Compare SHA values between GitHub and Y drive
  gh_sha_list <- lapply(gh, function(entry) {
    list(
      gh_sha = get_gh_sha(entry),
      y_sha  = entry$gh_raw_sha
    )
  })

  #if (verbose) cli::cli_alert_info("GitHub and Y drive SHAs: {gh_sha_list}")

  # Compute function SHA
  fun_sha     <- digest::digest(body(paste0("aux_", measure)))
  raw_fun_sha <- qs::qattributes(y_file_path)$raw_sha_fun


  if (verbose) {
    if (fun_sha == raw_fun_sha) {
      cli::cli_alert_success("Computed and stored function SHAs match. No update needed.")
    } else {
      cli::cli_alert_danger("Computed and stored function SHAs do NOT match. An update is required.")
    }
  }

  # Determine if Y drive needs an update
  update_y <- any(vapply(gh_sha_list,
                         function(x) x$gh_sha != x$y_sha, logical(1))) ||
                                     !(fun_sha == raw_fun_sha)

  update_y <- ifelse(is.na(update_y),
                     FALSE,
                     update_y)  # Treat NA as FALSE
#
#   if (verbose) {
#     cli::cli_h1("Summary")
#     cli::cli_alert_info("Update GitHub: {.strong {update_gh}}")
#     cli::cli_alert_info("Update Y drive: {.strong {update_y}}")
#   }

  if (verbose) {
    summary_text <- c(
      "Summary",
      sprintf("Update GitHub: %s", cli::style_bold(update_gh)),
      sprintf("Update Y drive: %s", cli::style_bold(update_y))
    )
    cli::boxx(summary_text, border_style = "double", align = "left")
  }

  return(invisible(list(update_gh = update_gh,
              update_y  = update_y)))
}



########################## TEST #######################################################
#AUX FUNCTION TO PRINT SUMMARY
print_summary_box <- function(log_list) {
  if (length(log_list) == 0) {
    cli::cli_alert_warning("No measures were processed.")
    return(invisible(NULL))
  }

  df_summary <- data.table::rbindlist(
    lapply(names(log_list), function(measure) {
      res <- log_list[[measure]]
      data.table::data.table(
        Measure = measure,
        GitHub = if (isTRUE(res$update_gh)) "✔" else "✘",
        Y_Drive = if (isTRUE(res$update_y)) "✔" else "✘"
      )
    })
  )

  summary_text <- c(
    "Summary of processed measures:",
    paste0(
      sprintf("%-20s | GitHub: %s | Y Drive: %s",
              df_summary$Measure, df_summary$GitHub, df_summary$Y_Drive)
    )
  )

  cli::boxx(summary_text, padding = 1, border_style = "round", align = "left")
}
