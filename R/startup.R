# File to set up file structure and configuration during install/update

.check_eemanalyzeR_install <- function(install_dir = .user_data_dir()) {

  # Check if the directory exists and if it doesn't exist run the first install
  if (!dir.exists(install_dir)) {
    packageStartupMessage(
      "Installing eemanalyzeR package into: ", install_dir
    )
    dir.create(install_dir, recursive = TRUE)
    reset_user_config(.user_config_path())
  }
  # We always return the install_dir
  return(install_dir)
}


.check_user_config_exists <- function(user_config_file = .user_config_path()) {
   user_config <- tryCatch(
    suppressPackageStartupMessages(read_user_config(user_config_file)),
    error = function(cnd) {
      packageStartupMessage(
        "----STARTUP WARNING----\n",
        "No User Configuration Found.\n",
        "eemanalyzeR will use the default configuration.\n",
        "Please create user configuration file using `reset_user_config`"
      )
      return(FALSE)
    }
  )
  return(is.list(user_config))
}

.check_user_config_version <- function(user_config_file = .user_config_path()) {

  user_config <-  suppressPackageStartupMessages(read_user_config(user_config_file))

  # Logic for checking the user config package version
  if(is.null(user_config$package_version) ||
    is.na(user_config$package_version)    || 
    user_config$package_version != .eemanalyzeR_ver()) {
    packageStartupMessage(
      "NOTE: Your user configuration file is from an older version of eemanalyzeR.\n",
      "You may need to update your user config using `repair_user_config`"
    )

    # TODO - implement way to merge configs while keeping the old user values
  }

}

.validate_and_load_user_config <- function(user_config_file = .user_config_path()) {
  tryCatch(
    load_user_config(
    user_config_file),
    error = function(cnd) {
      packageStartupMessage(
      "----STARTUP WARNING----\n",
      "Invalid User Configuration at ", user_config_file,
      ". eemanalyzeR will use the default configuration.\n",
      "Please see warnings and repair user configuration file.\n"
      )
    }
  )

}

rlang::on_load({

  # 1) Check if the user data directory exists
  install_dir <- .check_eemanalyzeR_install()

  # 2) Check the user config exists
  user_config_exists <- .check_user_config_exists()

  if(user_config_exists) {
  # 3) Load and validate the user config
  .validate_and_load_user_config()
  # 4) Check the user config has the right version
  .check_user_config_version()
  }

  # Future - maybe check on qaqc stds?
})
