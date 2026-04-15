# # File to set up file structure and configuration during install/update


# TODO - how to tell if there's an install or an update?







rlang::on_load({

  #browser()
  # 1) Check if the user data directory exists
  # I don't want to fail if it doesn't exist
  if(!dir.exists(.user_data_dir())) {
    packageStartupMessage("Can't find user data directory.\n",
    .user_data_dir(), " does not exist")
    # TODO - should I create the user data directory?
    invisible(NULL)
  }

  # 2) Check if the user data directory has a valid configuration file
  withCallingHandlers(
    load_user_config(
    config_path = .user_data_dir()),
    warning = function(cnd) {
      packageStartupMessage(
        "Invalid User Configuration Found.
        Using eemanalyzeR default configuration.
        Please see warnings and fix user configuration file."
      )
      warnings()
    }
  )


  # )
  # If it's valid, load it
  # If there are any warnings, print them and message that the user config was not loaded

  # 3) Check that the user configuration matches the package version
  #trycatch()

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
