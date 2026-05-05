#' Set up and apply user defaults for data processing
#'
#' Reads the user generated YAML file (`user-config.yaml`) which stores the
#' user specified values for the arguments in [run_eems()] and applies them
#' to the processing session. This allows the user to specify
#' processing parameters that are maintained across R sessions.
#'
#' @param user_config_file The path to the file that stores the user config. Defaults to the yaml file in the default user data directory.
#' @param env the environment name to write to (defaults to the package environment)
#' @param ... for `edit_user_config` potential parameter names and values to apply to the user config. Only if interactive = FALSE
#' @param interactive for `edit_user_config` defaults to TRUE, which will open the user config file for manual editing.
#'                    If FALSE, it attempts to apply the values provided in `...`
#' 
#' @details
#' - **edit_user_config** opens up `user-config.yaml` for manual editing or applies the named arguments supplied as `...` to the user config file.
#' - **reset_user_config**  overwrites the data processing options in the user configuration file back to
#'                          the default configuration of the eemanalyzeR package. These defaults are documented in data.R under "default_config".
#'                          This function is provided in case the user has a malformed configuration file or wants to revert back to default processing
#'                          settings after experimenting with modifying the settings using `edit_user_config`.
#' - **read_user_config** will read the options from the user config path as an R object but does not apply to the session. 
#'                        This function is used as a helper to read what's in the stored user configuration file and return it as a 
#'                        list before validating the config and applying it to the current session. Usually the user wants to use 
#'                        `load_user_config` since that function reads the config, checks it, then applies it to the current session.
#' - **validate_user_config** reads the user config and checks the options are valid and warns the user about invalid options. It checks that all settings 
#'                            in the eemanalyzeR user config are valid options that match the template included with the package.
#' - **load_user_config** will apply the options from `user-config.yaml` to the current session.
#'
#' @returns
#' - **edit_user_config** returns a message that the user configuration has been edited.
#' - **reset_user_config** invisibly returns the reset default configuration settings as a named list.
#' - **read_user_config** invisibly returns the current user configuration as a named list.
#' - **validate_user_config** invisibly returns TRUE if the configuration is valid, otherwise returns an error
#' - **load_user_config** will apply the options from `user-config.yaml` to the current session.
#' 
#' @export
#' @md
#' @rdname user_config
#' @name user_config
#'
#' @examples
#' load_user_config()
edit_user_config <- function(..., interactive = TRUE) {
  default_user_config <- yaml::read_yaml(file.path(system.file("extdata", package = "eemanalyzeR"), "eemanalyzeR-config.yaml"))
  user_config_file <- .user_config_path()

  # if file doesn't exist, write template
  if (!file.exists(user_config_file)) {
    reset_user_config()
    }

  # Open in user's editor
  if(interactive){
    file.edit(user_config_file)
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
#' @rdname user_config
#' @export
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
#' @rdname user_config
#' @export
read_user_config <- function(user_config_file = .user_config_path()) {
  if(file.exists(user_config_file)){
    user_config <- yaml::read_yaml(user_config_file)
    packageStartupMessage("User configuration read from file:\n", user_config_file)
  } else {
    stop("User configuration not found.")
  }
  invisible(user_config)
}
#' @rdname user_config
#' @export
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
#' @rdname user_config
#' @export
load_user_config <- function(user_config_file = .user_config_path(), env = .pkgenv) {
  
  # First Validate the user config
  valid_user_config <- validate_user_config(user_config_file)

  # ONLY IF NO PROBLEMS
  # Bind the variables to the environment
  rlang::env_bind(env, config = valid_user_config)

  # invisibly return the completed configuration
  invisible(list_session_config())
}

