# File to set up file structure and configuration during install/update

.check_eemanalyzeR_install <- function(install_dir = .user_data_dir()) {

  # Check if the directory exists and if it doesn't exist run the first install
  if (!dir.exists(install_dir)) {
    packageStartupMessage(
      "Installing eemanalyzeR package into: ", install_dir
    )
    dir.create(install_dir, recursive = TRUE)
    create_user_config(install_dir)
  }
  # We always return the install_dir
  return(install_dir)
}


.check_user_config_exists <- function(install_dir = .user_data_dir()) {
   user_config <- tryCatch(
    suppressPackageStartupMessages(read_user_config(install_dir)),
    error = function(cnd) {
      packageStartupMessage(
        "----STARTUP WARNING----\n",
        "No User Configuration Found.\n",
        "eemanalyzeR will use the default configuration.\n",
        "Please create user configuration file using `create_user_config`"
      )
      return(FALSE)
    }
  )
  return(is.list(user_config))
}

.check_user_config_version <- function(install_dir = .user_data_dir()) {

  user_config <-  suppressPackageStartupMessages(read_user_config(install_dir))

  # Logic for checking the user config package version
  if(is.na(user_config$package_version) | user_config$package_version != .eemanalyzeR_ver()) {
    packageStartupMessage(
      "Your user configuration file is from an older version of eemanalyzeR. Processing options may have changed.\n",
      "Would you like to update the user configuration file?"
    )
    # TODO - is a menu really necessary?
    sel <- menu(c(
      "yes - update my configuration to the new version (you will lose any modified config options!)",
      "no - keep my configuration the same (this may result in a malformed configuration!)"
    ))
    if(sel == 1) create_user_config(install_dir)
    if(sel == 2) invisible(NULL)

    # TODO - implement way to merge configs while keeping the old user values
  }

}

.validate_and_load_user_config <- function(install_path = .user_data_dir()) {
  tryCatch(
    load_user_config(
    config_path = install_path),
    error = function(cnd) {
      packageStartupMessage(
      "----STARTUP WARNING----\n",
      "Invalid User Configuration Found.\n",
      "eemanalyzeR will use the default configuration.\n",
      "Please see warnings and fix user configuration file."
      )
    }
  )

}

rlang::on_load({

  # 1) Check if the user data directory exists
  install_dir <- .check_eemanalyzeR_install()

  # 2) Check the user config exists
  user_config_exists <- .check_user_config_exists(install_dir)

  if(user_config_exists) {
  # 3) Check the user config has the right version
  .check_user_config_version(install_dir)
  # 4) Load and validate the user config
  .validate_and_load_user_config(install_dir)
  }

  # Future - maybe check on qaqc stds?

##   - eemanalyzeR_config.yaml
# If these do exist, check the version match the package data.
  # If these match, load the package as normal, nothing is out of place.
  # If these don't match, that means you're probably installing a new version of the package,
  # So we'll need to do some housekeeping like checking the config for updates to defaults or new values.
# If these don't exist, write them from inst/extdata to the user app dir.
  # During this, we need to add the version number to the config
# TODO - we might be able to allow the user to install somewhere else in the future, but for now let's not




  # Load the user config and print a message if loading the config fails
  # tryCatch(load_user_config(),
  # error = function(e) {
  #   packageStartupMessage("Warning: Malformed User Configuration File stored on disk. User Configuration not loaded.\n",
  #   "Please edit user config using edit_user_config or reset to package defaults using reset_user_config")
  # })
  })





# # Ask the user if they want to install to the default eemanalyzeR directory
# # if they don't, they must specify a directory to install.
# test_install_dir <- "./install"

# # Try to load the user config

# # Check to see if the config has an install directory

# # Check to see if the config is properly formed (matches template)

# # Double check that the written install directory is actually in the right spot

# # If there is not user config found
# # Prompt on install to ask where to store user data
# # Should default to rappdirs::user_data_dir()
# choose_install_directory <- function(install_dir, overwrite = FALSE) {

#   response <- menu(
#     c("Accept Default Install Directory",
#       "Input Custom Directory"),
#       graphics = FALSE
#     )
#   if(response == 1) {
#     # Check if the default directory exists

#     # Warn user about overwriting

#     # Create the directory dir
#     #dir.create(install_dir)
#   } else if(response == 2) {
#     # Prompt user to input a directory as a text string
#     print("You chose to input a custom directory to install")
#     install_dir <- readline("Plase write your preferred install directory here: ")

#     # Check if the directory exists and warn about overwriting

#     # Create the directory where the user wants it
#     message("Installing to: ", install_dir)
#   }
#   invisible(install_dir)
# }
  
# eemanalyzeR_install_dir <- choose_install_directory(test_install_dir, overwrite = TRUE)

# # Write the user config to the eemanalyzeR_install_dir
