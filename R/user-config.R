#' Set up user defaults for data processing
#'
#' Opens up a YAML configuration file which stores the default arguments for
#' `eemanalyzeR`. This allows the user to change the package defaults by computer
#' customizing the processing steps in [run_eems] but also ensuring that all data
#' processed will use the same user defaults without needing to specify each time.
#'
#' @details
#' The defaults are stored in a YAML configuration file on the user data directory.
#' This function will open up the file so the text can be edited. To save new defaults simply
#' edit the file and save it. The arguments in this file will overwrite the defaults
#' set in the package.
#'
#' @returns Opens up `user-config.yaml`.
#' @export
#'
#' @examples
#' edit_config()
edit_config <- function() {
  user_dir <- rappdirs::user_data_dir("eemanalyzeR")
  if (!dir.exists(user_dir)) dir.create(user_dir, recursive = TRUE)

  defaults_file <- file.path(user_dir, "user-config.yaml")

  # if file doesn't exist, write template
  if (!file.exists(defaults_file)) {
    file.copy(file.path(system.file("extdata", package = "eemanalyzeR"), "eemanalyzeR-config.yml"),
              defaults_file)
  }

  # Open in user's editor
  if(is_interactive()){file.edit(defaults_file)}

  #TODO: after save update in pkgenv or just message to restart R?
}

#' Load user defaults
#'
#' @param config_path path the YAML file with user default values
#' @param env the environment name to write to
#'
#' @export
#' @examples
#' load_config()
load_config <- function(config_path = rappdirs::user_data_dir("eemanalyzeR"),
                        env = TRUE){
  # load built-in defaults
  config <- yaml::read_yaml(file.path(system.file("extdata", package = "eemanalyzeR"), "eemanalyzeR-config.yml"))

  # try to load user file
  defaults_file <- file.path(config_path, "user-config.yaml")
  if(file.exists(defaults_file)){
  user_config <- yaml::read_yaml(defaults_file)
  config <- utils::modifyList(config, user_config)
  }

  if(env){list2env(config, envir = .pkgenv)}else{
    return(config)

  }

}
