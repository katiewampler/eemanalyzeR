#' Set up and apply user defaults for data processing
#'
#' Reads the user generated YAML file (`user-config.yaml`) which stores the
#' user specified values for the arguments in [run_eems()] and applies them
#' to the package environment (`.pkgenv`). This allows the user to specify
#' processing parameters that are maintained across R sessions.
#'
#' @param ... potential paramater names and values to apply to the user config. Only if interactive = FALSE
#' @param interactive defaults to TRUE, which will open the user config file for manual editing.
#'                    If FALSE, it attempts to apply the values provided in `...`
#' 
#' @details
#' The defaults are stored in a YAML configuration file on the user data directory.
#' This function will open up the file so the text can be edited. To save new defaults simply
#' edit the file and save it. The arguments in this file will overwrite the defaults
#' set in the package.
#'
#' @returns
#' - **edit_user_config** opens up `user-config.yaml`.
#' - **load_user_config** will apply the user defaults from `user-config.yaml`
#' to the package environment
#'
#' @export
#' @md
#' @rdname user_config
#' @name user_config
#'
#' @examples
#'
#' load_user_config()
edit_user_config <- function(..., interactive = TRUE) {
  default_user_config <- yaml::read_yaml(file.path(system.file("extdata", package = "eemanalyzeR"), "eemanalyzeR-config.yaml"))
  user_config_file <- .user_config_path()

  # if file doesn't exist, write template
  if (!file.exists(user_config_file)) {
    file.copy(file.path(system.file("extdata", package = "eemanalyzeR"), "eemanalyzeR-config.yaml"),
              user_config_file)
  }

  # Open in user's editor
  if(interactive){
    file.edit(user_config_file)
    # TODO - figure out how to hold the script until the connection is closed
  } else {
    # Capture the varargs as a list
    newdefaults <- rlang::list2(...)
    # Add the new variables to the old config
    old_config <- suppressPackageStartupMessages(read_user_config())
    new_config <- utils::modifyList(old_config, newdefaults, keep.null = TRUE)

    # validate and then write the yaml out
    problem_msg <- .validate_config(
     new_config,
     default_user_config
    )
    anyproblems <- any(!sapply(problem_msg, is.null, simplify = TRUE))
    if(anyproblems) {
      stop("Error: Bad options applied to config. New config not saved. See warning messages above for details.")
    } else{ 
      write_yaml(new_config, user_config_file)
    }
  }
  packageStartupMessage("Changes to user configuration applied, please re-load the new user config")
}



#' Reset all eemanalyzeR settings in the user configuration file to package defaults
#'
#' This allows the user to overwrite the data processing settings in the user configuration file back to
#' the default configuration of the eemanalyzeR package. These defaults are documented in data.R under "default_config".
#' This function is provided in case the user has a malformed configuration file or wants to revert back to default processing
#' settings after experimenting with modifying the settings using `edit_user_config`.
#' 
#' @param user_config_file The path to the file that stores the user config. Defaults to the yaml file in the default user data directory.
#'
#' @returns Invisibly returns the reset default configuration settings as a named list.
#' @export
#'
#' @examples
#' reset_user_config() #reset config file
#' load_user_config() #load config file
reset_user_config <- function(user_config_file = .user_config_path()) {
  # if file exists, back it up
  if (file.exists(user_config_file)) {
    file.rename(user_config_file, paste0(user_config_file, ".backup"))
  }
  file.copy(file.path(system.file("extdata", package = "eemanalyzeR"), "eemanalyzeR-config.yaml"),
          user_config_file)
  # Copy the installed package version into the config
  suppressPackageStartupMessages(edit_user_config(package_version = .eemanalyzeR_ver(), interactive = FALSE))

  packageStartupMessage("Created user configuration file: ", user_config_file)

}

# TODO needs documentation
# Reads user config but doesn't apply to session. Returns as list or throws error if not found
read_user_config <- function(user_config_file = .user_config_path()) {
  if(file.exists(user_config_file)){
    user_config <- yaml::read_yaml(user_config_file)
    packageStartupMessage("User configuration read from file:\n", user_config_file)
  } else {
    stop("User configuration not found.")
  }
  return(user_config)
}

#' Validate the eemanalyzeR configuration
#'
#' Checks that all settings in the eemanalyzeR user config are valid options that match the 
#' template included with the package.
#'
#' @param user_config_file The path to the file that stores the user config. Defaults to the yaml file in the default user data directory.
#'
#' @returns invisibly returns TRUE if the configuration is valid, otherwise returns an error
#'
#' @export
#' @examples
#' # Example validation
#' validate_user_config()
validate_user_config <- function(user_config_file = .user_config_path()) {
  # Get the default config template from the system files
  default_user_config <- yaml::read_yaml(file.path(system.file("extdata", package = "eemanalyzeR"), "eemanalyzeR-config.yaml"))
  # Read in the config
  current_user_config <- read_user_config(user_config_file)

  # Validate the config
  problem_msg <- .validate_config(
    current_user_config,
    default_user_config
  )
  anyproblems <- any(!sapply(problem_msg, is.null, simplify = TRUE))
  if(anyproblems) {
    stop("Error: Malformed user configuration in ", user_config_file, "\nSee warning messages above for details.")
  }  
  # If it's valid, return the user config, otherwise print the error messages
  invisible(current_user_config)

}


#' @param user_config_file The path to the file that stores the user config. Defaults to the yaml file in the default user data directory.
#' @param env the environment name to write to
#'
#' @export
#' @rdname user_config
#' @name user_config

# NOTE: This effectively returns everything back to package defaults if the user config can't be found.
# Is that what we want? It might overwrite settings if the user changed them before trying to load the user config.
# I'm ok with this as long as it's documented behavior
load_user_config <- function(user_config_file = .user_config_path(), env = .pkgenv) {
  
  # First Validate the user config
  valid_user_config <- validate_user_config(user_config_file)

  # ONLY IF NO PROBLEMS
  # Bind the variables to the environment
  rlang::env_bind(env, config = valid_user_config)

  # invisibly return the completed configuration
  invisible(list_session_config())
}

