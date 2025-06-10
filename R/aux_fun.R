#' General auxiliary function
#'
#' Update any auxiliary data
#'
#' This function updates any auxiliary data.
#' It ensures that dependencies are processed before handling the specified measure and checks whether updates
#' are needed for GitHub or the Y drive.
#'
#' @param measure character: Name of the measure to process
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
#' @param log logical, defaults to TRUE. Enables or disables logging in "pipaux_log"
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
                    repo      = paste0("aux_", measure),
                    owner     = getOption("pipfun.ghowner"),
                    maindir   = getOption("pipaux.working_dir"),
                    processed = new.env(parent = emptyenv()),
                    force     = FALSE,
                    tag       = NULL,
                    log       = TRUE,
                    log_overwrite = FALSE,
                    verbose   = FALSE,
                    ...) {

  # Set arguments

  # Get working release
  # Check if wrk_release exists

  pipfun::get_wrk_release(verbose = verbose)

  release        <- wrk_release$release
  identity       <- wrk_release$identity
  release_branch <- paste0(release, "_", identity)

  if (is.null(tag)) {
    tag <- release_branch
  }

  # Initialize log only at top level
  if (sys.nframe() <= 2 && log) {

    if (log_overwrite) {
      pipfun::log_init("pipaux_update_log",
                       overwrite = log_overwrite)
    } else skip

  }

  # Set repo to "Class" if measure is "income_groups" or "country_list"
  repo  <- if (measure %in% c("income_groups",
                              "country_list")) "Class" else repo

  owner <- fifelse(measure == "nan",
                   "PIP-Technical-Team",
                   owner)


  # If measure has already been processed, skip it
  if (rlang::env_has(processed,
                     measure)) {
    if (verbose) cli::cli_alert_info("{measure} has already been processed, skipping dependencies...")

  } else {
    # Mark this measure as processed
    rlang::env_poke(processed,
                    measure,
                    TRUE)

    # Log START ####
    # ~~~~~~~~~~~~~~~~ #

    if (log) {
      pipfun::log_add(
        event   = "info",
        message = cli::col_magenta(paste0("Start processing measure: ", measure)),
        name    = "pipaux_update_log",
        logmeta = list(step = "START", measure = measure)
      )

    }

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


    # Recursively process dependencies
    for (i in seq_along(dependencies)) {

      dep <- dependencies[i]


      tryCatch(
        {
          aux_fun(
            measure   = dep,
            #action    = "update",
            repo      = paste0("aux_", dep),
            owner     = owner,
            maindir   = maindir,
            processed = processed,
            force     = force,
            tag       = tag,
            verbose   = verbose,
            ...
          )
        },
        error = function(e) {

          # Log ERROR ####
          # ~~~~~~~~~~~~~~~~ #

          if (log) {

            pipfun::log_add(
              event   = "error",
              message = paste0("Failed to run aux_fun for: ", dep),
              name    = "pipaux_update_log",
              logmeta = list(measure = dep,
                             error   = e$message)

            )
          }
        }
      )
    } # end of dependencies loop



  # Check update status for the current measure

    # Log CHECK ####
    # ~~~~~~~~~~~~~~~~ #

    check_status_result <- tryCatch(
      {
        result <- check_status(
          measure = measure,
          repo    = repo,
          owner   = owner,
          maindir = maindir,
          verbose = verbose
        )

        if (log) {
          pipfun::log_add(
            event          = "status_check",
            message        = cli::col_green(paste0("Check status completed for: ", measure)),
            name           = "pipaux_update_log",
            output         = result,
            logmeta        = list(
              step         = "CHECK",
              measure      = measure
            )
          )
        }

        result
      },

      error = function(e) {

        if (log) {
          pipfun::log_add(
            event   = "error",
            message = paste0("Check failed for: ", measure, " - ", e$message),
            name    = "pipaux_update_log",
            logmeta = list(
              step    = "CHECK",
              measure = measure
            )
          )
        }
        NULL
      }
    )

    # Only proceed if check_status_result is not NULL
    if (!is.null(check_status_result)) {
      update_gh <- check_status_result$update_gh
      update_y  <- check_status_result$update_y
    }



  if (!update_gh && !update_y) {

    # Log END ####
    # ~~~~~~~~~~~~~~~~ #

    if (log) {
      pipfun::log_add(
        event   = "update",
        message = cli::col_blue(paste0("No update needed for: ", measure)),
        name    = "pipaux_update_log",
        logmeta = list(step    = "END",
                       measure = measure)
      )
    }

    cli::cli_alert_success("No updates needed")

    if (log) {
      cli::cli_alert_success(
        paste0(
          "Log available:",
          cli::bg_br_cyan(cli::col_black("{.strong pipaux_update_log}")),
          "\n",
          "Use {.code pipfun::log_get()} to access it"

        )
      )
    }

    return(invisible(NULL))
  }

  # Update: GH first and then Y: folder
  if (update_y) {

    # Update GitHub if necessary
    if (update_gh) {

      pipfun::sync_release_branch(
        owner         = owner,
        repo          = repo,
        ref_branch    = "DEV",
        target_branch = release_branch,
        verbose       = verbose
      )

      # Log SUCCESS ####
      # ~~~~~~~~~~~~~~~~ #

      if (log) {
        pipfun::log_add(
          event   = "update",
          message = cli::col_blue(paste0("Updated GitHub for: ", measure)),
          name    = "pipaux_update_log",
          logmeta = list(step = "UPDATE GH", measure = measure)
        )
      }
    }

    # Retrieve and execute the function from the pipaux namespace
    function_name <- paste0("aux_",
                            measure)

    if (!exists(function_name, envir = asNamespace("pipaux"))) {
      cli::cli_abort(paste0("Function '", function_name, "' does not exist in the 'pipaux' package."))
    }

    # check again where this is taken from !!
    func <- get(function_name,
                envir = asNamespace("pipaux"))

    # Build a list of all possible arguments to pass
    all_args <- c(
      list(
        action  = "update",
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


    tryCatch({

      do.call(func, filtered_args)

      if (log) {
        pipfun::log_add(
          event   = "update",
          message = cli::col_blue(paste0("Updated Y drive for: ", measure)),
          name    = "pipaux_update_log",
          logmeta = list(step = "UPDATE SERVER",
                         measure = measure)
        )
      }
    }, error = function(e) {

      if (log) {
        pipfun::log_add(
          event   = "error",
          message = paste0("Error updating Y drive for: ", measure, " — ", e$message),
          name    = "pipaux_update_log",
          logmeta = list(step = "UPDATE SERVER",
                         measure = measure)
        )
      }
    })


  } # Close if update Y is TRUE

  } # close else


  # Log END ####
  # ~~~~~~~~~~~~~~~~ #

  if (sys.nframe() <= 2 && log) {
    pipfun::log_add(
      event   = "success",
      message = cli::col_cyan("Measure and all its dependencies successfully updated"),
      name    = "pipaux_update_log",
      logmeta = list(step = "END")
    )


    cli::cli_alert_success(
      paste0(
        "Log available:",
        cli::bg_br_cyan(cli::col_black("{.strong pipaux_update_log}")),
        "\n",
        "Use {.code pipfun::log_get()} to access it"

      )
    )

  }

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
    pipfun::get_wrk_release(verbose = FALSE)
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

    if (verbose) {
      cli::cli_h1("Summary")
      cli::cli_alert_info("Update GitHub: {.strong {update_gh}}")
      cli::cli_alert_info("Update Y drive: {.strong {update_y}}")
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

  if (verbose) {
    cli::cli_h1("Summary")
    cli::cli_alert_info("Update GitHub: {.strong {update_gh}}")
    cli::cli_alert_info("Update Y drive: {.strong {update_y}}")
  }

  return(invisible(list(update_gh = update_gh,
              update_y  = update_y)))
}

# Wrapper test 1 ####################################

update_all_aux <- function(measures = NULL,
                           verbose  = FALSE,
                           log      = TRUE,
                           log_save = FALSE,
                           ...) {

  if (!rlang::env_has(.GlobalEnv, "wrk_release")) {
    pipfun::get_wrk_release(verbose = FALSE)
  }

  release        <- wrk_release$release
  identity       <- wrk_release$identity
  release_branch <- paste0(release, "_", identity)

  # Initialize log only at top level
  if (sys.nframe() <= 2 && log) {

    pipfun::log_init("pipaux_update_log",
                     overwrite = T)

  }

  # Add log ####

  # Optional: define all known measures if not provided
  all_measures <- gh::gh("GET /users/{username}/repos",
                         username = getOption("pipfun.ghowner")) |>
    vapply("[[", "", "name") |>
    grep("^aux_", x = _, value = TRUE) |>
    (\(x) sub("^aux_", "", x))()

  # Filter measures if user provides a subset
  if (!is.null(measures)) {
    all_measures <- all_measures[all_measures %in% measures]
  }

  # Track update status for each measure
  status <- lapply(all_measures,
                   function(msr) {

    tryCatch({
      aux_fun(measure   = msr,
              verbose   = verbose,
              log       = log,
              ...) # Additional args passed through update_all_aux

      return(list(measure = msr,
                  success = TRUE,
                  error = NULL))

    }, error = function(e) {
      return(list(measure = msr,
                  success = FALSE,
                  error = e$message))
    })
  })

  # Save log if log save is TRUE
  pipfun::log_save(name = "pipaux_update_log",
                   path = fs::path(getOption("pipaux.log_directory"),
                                   release_branch,
                                   "pipaux_update_log"))


  invisible(status)
}


