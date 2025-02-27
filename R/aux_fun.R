#' Call auxiliary function for a specified measure
#'
#' @param measure character: Name of the auxiliary data measure (e.g., "ppp")
#' @param ... additional arguments to pass to the auxiliary function
#'
#' @return The result of the auxiliary function call
aux_fun_v0 <- function(measure,
                    action = c("update", "load"),
                    repo = paste0("aux_", measure),
                    branch = paste0(release, "_", identity),
                    owner,
                    release,
                    identity,
                    maindir = getOption("pipaux.working_dir"),
                    ...) {

  measure <- measure
  action <- match.arg(action)
  release_branch <- paste0(release, "_", identity)

  function_name <- paste0("aux_",
                          measure)

  # This checks if the function exists in the {pipaux} package namespace
  # -- The namespace corresponds to the version currently loaded in the R session
  # -- (development version loaded via devtools::load_all() or the installed version)

  if (!exists(function_name,
              envir = asNamespace("pipaux"))) {
    cli::cli_abort(paste0("Function '", function_name, "' does not exist in the '", "pipaux package."))
  }

  #   ____________________________________________________________
  #   Read Dependencies                                       ####

  dependencies <- read_dependencies(gh_user = "https://raw.githubusercontent.com",
                                    owner   = "PIP-Technical-Team")[[measure]]


  # Extract the relevant measure's dependencies
  # if (!measure %in% names(dependencies)) {
  #   cli::cli_abort(paste0("Measure '", measure, "' not found in dependencies."))
  # }
  #

  #   ____________________________________________________________
  #   Recursively call aux_fun for each dependency            ####

  if (length(dependencies) >= 1) {

    lapply(dependencies, function(dep) {
      aux_fun_v0(measure  = measure,
              action   = action,
              repo     = repo,
              branch   = release_branch,
              owner    = owner,
              release  = release,
              identity = identity,
              maindir  = maindir,
              ...)
    })
  }

  #   ____________________________________________________________
  #   Check update status of measure, both in GH and Y:       ####

  check_status <- check_status(measure = measure,
                               repo    = repo,
                               owner   = owner,
                               maindir = maindir)

  update_gh <- check_status$update_gh
  update_y  <- check_status$update_y

  if (!update_gh & !update_y) {
    cli::cli_alert_info("No action required. GitHub and Y drive already up to date")
    return(NULL)
  }

  if (update_y == TRUE) {

    #   ____________________________________________________________
    #   First check and update GitHub                           ####

    if(update_gh == TRUE) {

      pipfun::sync_release_branch(owner      = owner,
                                  repo       = repo,
                                  ref_branch = "DEV",
                                  release    = release,
                                  identity   = identity)

    }

    #   ____________________________________________________________
    #   Update Y drive                                          ####

    # Retrieve the function from the {pipaux} namespace
    # -- The namespace refers to the version currently loaded in the R session

    func <- get(function_name,
                envir = asNamespace("pipaux"))

    # Call the function with additional arguments
    func(action  = action,
         maindir = maindir,
         owner   = owner,
         branch  = release_branch)

  }
}

# Refactoring aux fun attempt one
aux_fun_v1 <- function(measure,
                        action    = c("update", "load"),
                        repo      = paste0("aux_", measure),
                        #branch    = paste0(release, "_", identity),
                        owner     = "RossanaTat",
                        release   = working_release$release,
                        identity  = working_release$identity,
                        maindir   = getOption("pipaux.working_dir"),
                        processed = new.env(parent = emptyenv()),
                        force     = FALSE,
                        tag       = match.arg(branch),
                        ...) {

  action         <- match.arg(action)
  release_branch <- paste0(release, "_", identity)

  # repo <- repo
  #
  #
  # # Set repo to "Class" if measure is "income_groups" or "country_list"
  # if (measure %in% c("income_groups", "country_list")) {
  #   repo <- "Class"
  # }

  repo <- if (measure %in% c("income_groups", "country_list")) "Class" else repo


  # If measure has already been processed, return early
  if (rlang::env_has(processed,
                     measure)) {
    return(invisible(NULL))
  }

  # Mark this measure as processed
  rlang::env_poke(processed,
                  measure,
                  TRUE)

  # Debug MSG on processing of the current measure
  cli::cli_alert_info("Processing measure: {measure}")

  # Read all dependencies
  # _______________________________________________________ #####

  dependencies_all <- read_dependencies(
    gh_user = "https://raw.githubusercontent.com",
    owner   = "PIP-Technical-Team"
  )

  # Get dependencies for this measure; if none, default to an empty vector
  dependencies <- dependencies_all[[measure]]

  if (is.null(dependencies)) {
    dependencies <- character(0)
  }

  # Recursively process dependencies using lapply
  # _______________________________________________________ #####

  invisible(lapply(dependencies, function(dep) {
    aux_fun_v1(measure   = dep,
                action    = action,
                repo      = repo,
                branch    = release_branch,
                owner     = owner,
                release   = release,
                identity  = identity,
                maindir   = maindir,
                processed = processed,
                force     = force,
                tag       = tag,
                ...)
  }))

  # Check update status for the current measure
  # _______________________________________________________ #####
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

  # Update - GH first and then Y:
  # _______________________________________________________ #####


  if (update_y) {

    # Update GitHub if necessary

    if (update_gh) {
      pipfun::sync_release_branch(owner      = owner,
                                  repo       = repo,
                                  ref_branch = "DEV",
                                  release    = release,
                                  identity   = identity)
    }

    # Retrieve and execute the function from the pipaux namespace ######

    # Check if the function exists in the pipaux namespace
    function_name <- paste0("aux_", measure)

    if (!exists(function_name, envir = asNamespace("pipaux"))) {
      cli::cli_abort(paste0("Function '", function_name, "' does not exist in the 'pipaux' package."))
    }

    func <- get(function_name,
                envir = asNamespace("pipaux"))

    func(action  = action,
         maindir = maindir,
         owner   = owner,
         branch  = release_branch,
         force   = force,
         tag     = tag)
  }

  invisible(NULL)
}

# aux function new version
aux_fun_new <- function(measure,
                        action    = c("update", "load"),
                        repo      = paste0("aux_", measure),
                        owner     = getOption("pipfun.ghowner"),
                        #branch    = paste0(release, "_", identity),
                        maindir   = getOption("pipaux.working_dir"),
                        processed = new.env(parent = emptyenv()),
                        force     = FALSE,
                        tag       = NULL,
                        ...) {

  # Set arguments
  action         <- match.arg(action)

  # Get working release
  pipfun::get_wrk_release()

  release        <- wrk_release$release
  identity       <- wrk_release$identity
  release_branch <- paste0(release, "_", identity)

  if (is.null(tag)) {
    tag <- release_branch
  }

  # DEBUG STATEMENT
  #print(release_branch)

  # Set repo to "Class" if measure is "income_groups" or "country_list"
  repo  <- if (measure %in% c("income_groups", "country_list")) "Class" else repo

  # If measure has already been processed, skip it
  if (rlang::env_has(processed, measure)) {
    cli::cli_alert_info("{measure} has already been processed, skipping dependencies...")

  } else {
    # Mark this measure as processed
    rlang::env_poke(processed, measure, TRUE)

    # Debug message on processing the current measure
    cli::cli_alert_info("Processing measure: {measure}")

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
          aux_fun_new(
            measure   = dep,
            action    = action,
            repo      = paste0("aux_", dep),
            #branch    = release_branch,
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
    } # end of dependencies for loop
  }

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

      # use branch instead of release and identity - pipfun is for functions to be used across all PIP!!
      pipfun::sync_release_branch(
        owner      = owner,
        repo       = repo,
        ref_branch = "DEV",
        release    = release,
        identity   = identity
      )
    }

    # Retrieve and execute the function from the pipaux namespace
    function_name <- paste0("aux_", measure)

    if (!exists(function_name, envir = asNamespace("pipaux"))) {
      cli::cli_abort(paste0("Function '", function_name, "' does not exist in the 'pipaux' package."))
    }

    # check again where this is taken from !!
    func <- get(function_name, envir = asNamespace("pipaux"))

    # Build a list of all possible arguments to pass
    all_args <- c(
      list(
        action  = action,
        maindir = maindir,
        branch  = release_branch,
        force   = force,
        owner = owner,
        tag = tag,
        repo = repo
      ))  # include additional arguments ?

    # check if instead of filtering you can use ...
    # Retrieve the formal arguments of the function
    formal_args <- names(formals(func))

    # Filter to include only matching arguments
    filtered_args <- all_args[names(all_args) %in% formal_args]

    # Call the function with the filtered arguments
    do.call(func, filtered_args)
  }

    # func(
    #   action  = action,
    #   maindir = maindir,
    #   #owner   = owner,
    #   branch  = release_branch,
    #   force   = force,
    #   ...
    #   #tag     = tag
    # )


  invisible(NULL)
}




# MEMO: ADD DOCUMENTATION
## Check status ####
check_status <- function(measure,
                         repo       = paste0("aux_", measure),
                         owner      = getOption("pipfun.ghowner"),
                         maindir    = getOption("pipaux.working_dir"),
                         #release,
                         #identity,
                         verbose    = TRUE) {

  # Get working release
  pipfun::get_wrk_release()

  release  <- wrk_release$release
  identity <- wrk_release$identity

  release_branch <- paste0(release, "_",
                           identity)

  update_gh <- TRUE

  gh_branches <- tryCatch(
    pipfun::get_repo_branches(owner = owner, repo = repo),
    error = function(e) {
      if (verbose) cli::cli_alert_danger("GitHub repository not found or an error occurred: {e$message}")
      return(NULL)
    }
  )

  # If GitHub repo is not found, set update_gh to FALSE
  if (is.null(gh_branches)) {
    update_gh <- FALSE
  } else if (release_branch %in% gh_branches$release_branches) {

    # TO DO: MODIFY THIS, OR PIPFUN, TO CHECK THAT THE WORKING RELEASE BRANCH IS THERE -> done
    #release_branch <- gh_branches$release_branch

    release_up_to_date <- pipfun::compare_branch_content(
      owner    = owner,
      repo     = paste0("aux_", measure),
      branch1  = "DEV",
      branch2  = release_branch
    )$same_content

    update_gh <- !release_up_to_date
  }

  if (update_gh) {

    return(list(update_gh = update_gh, update_y = TRUE))

  } else {

    y_file_path <- fs::path(maindir, "aux_data", release_branch, measure, measure, ext = "qs")

    if (verbose) cli::cli_alert_info("Checking file: {y_file_path}")

    if (!fs::file_exists(y_file_path)) {

      cli::cli_alert_danger("File {y_file_path} does not exist.")
      return(list(update_gh = update_gh, update_y = TRUE))

    } else {
      gh <- qs::qattributes(y_file_path)$gh

      if (length(gh) > 0 && !is.list(gh[[1]])) {
        gh <- list(gh_list = gh)
      }

      get_gh_sha <- function(gh_entry) {
        tryCatch(
          pipfun::get_file_info_from_gh(
            owner    = gh_entry$owner,
            repo     = gh_entry$repo,
            branch   = gh_entry$branch,
            file_path = gh_entry$file_path
          )$sha,

          error = function(e) {
            if (verbose) cli::cli_alert_danger("File not found or another error occurred: {e$message}")
            NULL
          }
        )
      }

      gh_sha_list <- lapply(gh, function(entry) {
        list(
          gh_sha = get_gh_sha(entry),
          y_sha  = entry$gh_raw_sha
        )
      })

      if (verbose) cli::cli_alert_info("GitHub and Y drive SHAs: {gh_sha_list}")

      # specify algo being used
      fun_sha     <- digest::digest(body(paste0("aux_", measure)))
      raw_fun_sha <- qs::qattributes(y_file_path)$raw_sha_fun

      if (verbose) {
        cli::cli_alert_info("Computed function SHA: {fun_sha}")
        cli::cli_alert_info("Stored function SHA: {raw_fun_sha}")
      }

      # use vapply, not sapply
      update_y <- any(vapply(gh_sha_list, function(x) x$gh_sha != x$y_sha,
                             logical(1))) ||
        !(fun_sha == raw_fun_sha)

      # Treat NA as a FALSE - TO CHECK
      update_y <- ifelse(is.na(update_y), FALSE, update_y)

      return(list(update_gh = update_gh, update_y = update_y))
    }
  }
}





