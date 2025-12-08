#' Set up and apply user defaults for data processing
#'
#' Reads the user generated YAML file (`user-config.yaml`) which stores the
#' user specified values for the arguments in [run_eems()] and applies them
#' to the package environment (`.pkgenv`). This allows the user to specify
#' processing parameters that are maintained across R sessions.
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
#'
#' @examples
#' edit_user_config()
#'
#' load_user_config()
edit_user_config <- function() {
  user_dir <- rappdirs::user_data_dir("eemanalyzeR")
  if (!dir.exists(user_dir)) dir.create(user_dir, recursive = TRUE)

  defaults_file <- file.path(user_dir, "user-config.yaml")

  # if file doesn't exist, write template
  if (!file.exists(defaults_file)) {
    file.copy(file.path(system.file("extdata", package = "eemanalyzeR"), "eemanalyzeR-config.yaml"),
              defaults_file)
  }

  # Open in user's editor
  if(is_interactive()){file.edit(defaults_file)}

  #apply user edited configuration
  load_user_config()

  message("User configuration applied.")
}

#' @param config_path path the YAML file with user default values
#' @param env the environment name to write to
#'
#' @export
#' @rdname user_config
load_user_config <- function(config_path = rappdirs::user_data_dir("eemanalyzeR"),
                        env = .pkgenv){
  # load built-in defaults
  config <- yaml::read_yaml(file.path(system.file("extdata", package = "eemanalyzeR"), "eemanalyzeR-config.yaml"))

  # try to load user file
  defaults_file <- file.path(config_path, "user-config.yaml")
  if(file.exists(defaults_file)){
  user_config <- yaml::read_yaml(defaults_file)
  config <- utils::modifyList(config, user_config)
  }

  #modify in this session
  modify_config(env = env, !!!config)

}
