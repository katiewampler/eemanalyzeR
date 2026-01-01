# Functions around setting up a processing environment
# Need to make these so it cleans up the processing code and makes passing
# function arguments much more clear.


# Create an environment to store EEMS processing arguments and parameters
default_config <- yaml::read_yaml(file.path(
  system.file("extdata", package = "eemanalyzeR"),
  "eemanalyzeR-config.yaml"
))

.pkgenv <- rlang::new_environment(data = list(config = default_config), parent = rlang::empty_env())

#' List current eemanalyzeR configuration
#'
#' Returns a named list of all configuration options for eemanalyzeR data processing.
#'
#' eemanalyzeR allows the user to control data processing via configuration settings that modify
#' various steps of the automated data processing code. This function allows the user to see values
#' of the processing configuration for the current processing environment. Note: This configuration
#' resets to either the package defaults (stored in package data as eemanalyzeR-config.yaml) or in
#' the user's default settings (stored in a separate yaml file on user's computer) when the package
#' is re-loaded (such as via `library(eemanalyzeR)`)
#'
#' @param env the environment to search for the configuration settings. Defaults to the package
#'        environment (.pkgenv). It is unlikely the user would change this default unless debugging custom
#'        processing scripts.
#' @md
#' @returns A named list with the configuration setting names and their values.
#' @export
#' @examples
#' # Get current configuration options
#' current_settings <- list_config()
list_config <- function(env = .pkgenv) {
  rlang::env_get(env, "config")
}

#' Validate the eemanalyzeR configuration
#'
#' Checks that all settings in the eemanalyzeR config are valid options and warns user if not
#'
#' @param env environment where to find the config. Defaults to the package environment
#' 
#' @returns invisibly returns TRUE if the configuration is valid, otherwise returns an error
#' 
#' @export
#' @examples
#' # Example validation
#' validate_config()
validate_config <- function(env = .pkgenv) {
  # Get the default config template
  default_config_modes <- lapply(default_config, mode)
  default_config_modes <- default_config_modes[order(names(default_config_modes))]
  default_config_lengths <- lapply(default_config, length)
  default_config_lengths <- default_config_lengths[order(names(default_config_lengths))]

  # Read in the config
  current_config <- list_config(env)
  # Get the current configuration modes
  config_modes <- lapply(current_config, mode)
  config_modes <- config_modes[order(names(config_modes))]
  # Get the current configuration lengths
  config_lengths <- lapply(current_config, length)
  config_lengths <- config_lengths[order(names(config_lengths))]

  # Compare the Modes first then lengths
  stopifnot(identical(default_config_modes, config_modes) &
    identical(default_config_lengths, config_lengths))

  invisible(TRUE)
}

#' Reset all eemanalyzeR settings to package defaults
#'
#' Returns the data processing configuration settings for this R session back to package defaults.
#'
#' This allows the user to return the data processing settings back to the default configuration of the
#' eemanalyzeR package. These defaults are documented in data.R under "default_config". This function is
#' provided in case the user needs to return back to default processing settings after experimenting with
#' modifying the settings using `modify_config`.
#'
#' @param env the environment that stores the processing settings. Defaults to the package environment.
#'            It is not recommended the user modifies this argument.
#'
#' @returns Invisibly returns the reset default configuration settings as a named list.
#' @export
#'
#' @examples
#' # Reset the configuration back to package defaults
#' reset_config()
reset_config <- function(env = .pkgenv) {
  rlang::env_bind(env, config = default_config)
  invisible(list_config(env = env))
}


#' Modify the eemanalyzeR configuration settings
#'
#' Allows the user to modify configuration settings that affect data processing through the
#' remainder of the current processing session (i.e. until the package is reloaded via
#' `library(eemanalyzeR)` or R is restarted)
#'
#' @details There are two ways the user can provide name-value pairs of settings that they want to modify.
#' The first and most useful way is to provide them as named arguments directly to this
#' function via the ... variable arguments. To do this the user would provide the arguments
#' as name = value separated by commas if multiple settings are to be changed at the same time
#'
#' Another option is to provide a named list of name = value pairs. In this case the named
#' list MUST be prefixed by the `!!!` splice operator.
#'
#' @param ... the names of settings the user wishes to modify with their new values.
#'        NOTE: these MUST be named arguments, otherwise the modification of all settings
#'        will fail.
#' @param env the environment in which configuration settings is stored. Defaults to the package
#'        environment. Note: we do not recommend the user modifies this argument.
#' @returns Invisible returns the named list of the new defaults, but this function is called mostly
#'          for it's side effects.
#' @export
#' @examples
#' # Modify the cuvette length to two centimeters
#' modify_config(cuvle = 2)
#'
#' # Two methods to modify multiple defaults
#' # 1) via multiple named arugments
#' modify_config(cuvle = 2, eem_skip = "badeem")
#'
#' # 2) via a named list
#' modifications <- list(cuvle = 2, eem_skip = "badeem")
#' modify_config(!!!modifications)
#'
modify_config <- function(..., env = .pkgenv) {
  # Capture the varargs as a list
  newdefaults <- rlang::list2(...)
  # Assert the varargs the user wants to modify are valid names in the package environment
  not_matching_names <- names(newdefaults[which(!names(newdefaults) %in% .pkgenv_vars)])
  if (length(not_matching_names) > 0) {
    stop(simpleError(paste("Cannot modify default:", not_matching_names, " is not valid")))
  }
  # Add the new variables to the old config
  old_config <- list_config(env)
  new_config <- utils::modifyList(old_config, newdefaults, keep.null = TRUE)

  # Bind the variables to the environment
  rlang::env_bind(env, config = new_config)

  # Validate the config
  tryCatch(
    validate_config(env),
    error = function(e) {
      # Reset the config to the original values if not valid
      rlang::env_bind(env, config = old_config)
      stop("New config not valid. Reset to package defaults.")
    }
  )
  invisible(newdefaults)
}
