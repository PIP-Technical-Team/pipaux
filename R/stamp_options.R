#' @title pipaux: Helper wrappers for stamp versioning options
#' @description
#' Small helpers to manage `stamp`-based versioning behaviour used by pipaux.
#' These functions provide a simple, stable API for:
#' - switching between common versioning modes,
#' - reading the active mode, and
#' - resetting stamp options to the pipaux defaults.
#'
#' They intentionally keep the surface small and forward arguments to `stamp`
#' so the package can adopt stamp improvements without internal changes.
#'
#' @details
#' pipaux relies on the `stamp` package to manage auxiliary-data versioning.
#' The three supported high-level modes are:
#' - "content": only create a new version when data content or code changes (default)
#' - "timestamp": create a new version on every save
#' - "off": overwrite the current artifact (no versioning)
#'
#' Use `pipaux_set_stamp_option()` to forward lower-level, advanced options
#' directly to `stamp::st_opts()` when you need finer control.
#'
#' @examples
#' \dontrun{
#' # set behaviour to create a new version only when content or code changes
#' pipaux_set_versioning("content")
#'
#' # set behaviour to always create a new version (useful for debugging)
#' pipaux_set_versioning("timestamp")
#'
#' # temporarily disable versioning (overwrites files)
#' pipaux_set_versioning("off")
#'
#' # query current mode
#' pipaux_get_versioning()
#'
#' # set a lower-level stamp option (advanced)
#' pipaux_set_stamp_option(force_on_code_change = FALSE)
#'
#' # reset to pipaux defaults
#' pipaux_reset_stamp_options()
#' }
#'
#' @importFrom stamp st_opts st_opts_get st_opts
#' @name pipaux-stamp-options
NULL

#' Set pipaux versioning mode
#'
#' @title Set versioning mode
#' @description Choose a simple, high-level versioning mode for pipaux.
#'
#' @param mode Character: one of "content", "timestamp", or "off".
#'   - "content"   (default) Save only when content or code changes.
#'   - "timestamp" Save on every call.
#'   - "off"       Disable versioning (overwrite current file).
#'
#' @return Invisibly returns the chosen mode (character).
#' @export
pipaux_set_versioning <- function(mode = c("content", "timestamp", "off")) {
  # Validate argument against allowed options
  mode <- match.arg(mode)

  # Forward the high-level choice to stamp::st_opts()
  # Using st_opts keeps stamp handling internal and future-proof.
  stamp::st_opts(versioning = mode)

  # Return the mode invisibly for programmatic use.
  invisible(mode)
}


#' Get current pipaux versioning mode
#'
#' @title Get versioning mode
#' @description Return the active versioning mode configured in `stamp`.
#'
#' @return Character scalar describing current mode ("content", "timestamp", or "off").
#' @export
pipaux_get_versioning <- function() {
  # Read the option from stamp and return it directly.
  # Kept minimal on purpose: this is a thin accessor to stamp's options.
  stamp::st_opts_get()$versioning
}


#' Reset pipaux stamp options to defaults
#'
#' @title Reset pipaux stamp options
#' @description Restore stamp options to pipaux sensible defaults.
#' This is useful when tests or interactive exploration modify stamp config.
#'
#' @return Invisibly returns TRUE on completion.
#' @export
pipaux_reset_stamp_options <- function() {
  # Use st_opts_set to explicitly set defaults used across the package.
  # - versioning = "content": avoid spurious versions unless content/code changed
  # - retain_versions = Inf: keep all versions by default (adjust for disk if needed)
  # - force_on_code_change = TRUE & code_hash = TRUE: ensure code changes trigger versioning
  stamp::st_opts(
    versioning = "content",
    retain_versions = Inf,
    force_on_code_change = TRUE,
    code_hash = TRUE
  )

  invisible(TRUE)
}


#' Set stamp options (advanced)
#'
#' @title Forward advanced stamp options
#' @description Low-level helper to forward arguments directly to
#' `stamp::st_opts()` for advanced configuration. Most users should prefer
#' `pipaux_set_versioning()` for simple mode selection.
#'
#' @param ... Named options passed on to `stamp::st_opts()`.
#'
#' @return Invisibly returns TRUE on completion.
#' @export
pipaux_set_stamp_option <- function(...) {
  # Forward arbitrary stamp options. This keeps the wrapper small and flexible.
  stamp::st_opts(...)

  invisible(TRUE)
}
