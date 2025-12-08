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


# Create one big function with variable arguments to modify defaults
# TODO: document this with all the defaults -> see data.R
modify_config <- function(env = .pkgenv, ...) {
  # Capture the varargs as a list
  newdefaults <- rlang::list2(...)
  # Assert the varargs the user wants to modify are in the .pkgenv
  not_matching_names <- names(newdefaults[which(!names(newdefaults) %in% .pkgenv_vars)])
  if(length(not_matching_names) > 0) {
    stop(simpleError(paste("Cannot modify default:", not_matching_names, " is not valid")))
  }
  # Bind the variables to the environment
  rlang::env_bind(env, ...)
}

# Returns all the currently set eemanalyzer processing settings
# TODO Document this
list_config <- function(env = .pkgenv) {
  rlang::env_get_list(env, rlang::env_names(env))
}

# Reset all eemanalzyer settings to package defaults
# Reset all eemanalzyer settings to package defaults  ##NOTE: defaults don't seem to work here for env
reset_config <- function(env = .pkgenv) {
  modify_config(env,
  !!!default_config)
}

# TODO Create some function to validate the settings? Like which ones are numeric, logical, lists, etc?
# So if the user messes something up the error isn't thrown by the function that calls it but is caught e
# early
