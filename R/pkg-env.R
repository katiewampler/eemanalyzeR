# Functions around setting up a processing environment
# Need to make these so it cleans up the processing code and makes passing
# function arguments much more clear.

# TODO create 4 different ways for user to use processing environment variables
# 1) User doesn't do anything then the package defaults are used by run_eems
# 2) User creates a file (stored on their computer) that has processing defaults that
#    eemanalyzer pulls from at load time
# 3) User sets pkg environment defaults in session but that affects any processing
#    that occurs during that session. After package is reloaded the defaults are
#    restored to package defaults
# 4) User supplies arguments to run_eems function that modify processing ONLY during
#    that run.

# TODO Make function for user to get and set defaults for processing and
# save in same place as MDLs on user's computer for consistency

# Package processing defaults list
# TODO maybe this should be in a file? Or at least point it to a users file so it
# can modify defaults on package load --> saved in inst/ext_data as yaml and as package obj now
# TODO - document these defaults -> documented in data.R under  eemanalyzer_processing_defaults

# Create an environment to store EEMS processing arguments and parameters
default_config <- yaml::read_yaml(file.path(system.file("extdata", package = "eemanalyzeR"),
                          "eemanalyzeR-config.yaml"))

.pkgenv <- rlang::new_environment(data = default_config, parent = rlang::empty_env())


# TODO: document this with all the defaults -> see data.R
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
#' @returns invisible returns the named list of the new defaults, but this function is called mostly
#'          for it's side effects
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
  if(length(not_matching_names) > 0) {
    stop(simpleError(paste("Cannot modify default:", not_matching_names, " is not valid")))
  }
  # Bind the variables to the environment
  rlang::env_bind(env, ...)
  invisible(newdefaults)
}

#' List current eemanalyzeR configuration
#' 
#' Returns a named list of all configuration options for eemanalyzeR data processing.
#' 
#' eemanalyzeR allows the user to control data processing via configuration settings that modify
#' various steps of the automated data processing code. This function allows the user to see values
#' of the processing configuration for the current processing environment. Note: This configuration
#' resets to either the package defaults (stored in package data as eemanalyzeR-config.yaml) or in
#' the user's default settins (stored in a separate yaml file on user's computer) when the package
#' is re-loaded (such as via `library(eemanalyzeR)`)
#' 
#' @param env the environment to search for the configuration settings. Defaults to the package
#'        environment (.pkgenv). It is unlikely the user would change this default unless debugging custom
#'        processing scripts.
#' @md
#' @returns a named list with the configuration setting names and their values
#' @export
#' @examples
#' # Get current configuration options
#' current_settings <- list_config()
list_config <- function(env = .pkgenv) {
  rlang::env_get_list(env, rlang::env_names(env))
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
#' @returns invisibly returns the reset default configuration settings as a named list
#' @export
#' 
#' @examples
#' # Reset the configuration back to package defaults
#' reset_config()
# TODO - is there a way to make this work so that it resets to user defaults?
reset_config <- function(env = .pkgenv) {
  modify_config(!!!default_config, env = env)
  invisible(list_config(env = env))
}

# TODO Create some function to validate the settings? Like which ones are numeric, logical, lists, etc?
# So if the user messes something up the error isn't thrown by the function that calls it but is caught
# early
