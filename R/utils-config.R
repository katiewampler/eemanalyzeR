# TODO - document all these functions a little more
# Get the location of the user config file
.user_config_path <- function() {
  return(.pkgenv$user_config_path)
}
# Compare configuration lists
.validate_config <- function(
  config,
  template_config
) {

  # check all the options are valid
  invalid_options <- withCallingHandlers(
    .check_config_invalid_options(config, template_config)

  )
  
  # check all the required options are in the config
  missing_options <- withCallingHandlers(
    .check_config_missing_options(config, template_config)
  )

  # check all the matching options have the same type
  invalid_types <- withCallingHandlers(
    .check_config_option_types(config, template_config)
  )

  # check all the matching options have the same lengths (since some can be vectors)
  invalid_lengths <- withCallingHandlers(
    .check_config_option_lengths(config, template_config)
  )

  # Put them in a list?
  problem_list <- list(
    invalid_options = invalid_options,
    missing_options = missing_options,
    invalid_types   = invalid_types,
    invalid_lengths = invalid_lengths
  )
}

# Bunch of helper functions that return conditions
# Checking all the necessary options are in the config
.check_config_invalid_options <- function(config, template_config) {
  
  # Figure out which names don't match the defaults
  wrong_options_in_config <- names(config)[
    which(!names(config) %in% names(template_config))
  ]
    # TODO - what to return?
  if(length(wrong_options_in_config) > 0) {
    return(.config_option_invalid_warn(wrong_options_in_config))
  }
  invisible(NULL)
}

.check_config_missing_options <- function(config, template_config) {
  # Figure out which names don't match the defaults
  missing_options_from_config <- names(template_config)[
    which(!names(template_config) %in% names(config))
  ]
  if(length(missing_options_from_config) > 0) {
    return(.config_option_missing_warn(missing_options_from_config))
  }
  # Signal some conditions?
  invisible(NULL)
}

# Check that options are all valid types (ex: logical, numeric, text)
.check_config_option_types <- function(config, template_config) {
  # Subset the config to only include names that are in the template config
  config_matches <-  .align_config_to_template(config, template_config)
  default_matches <- .align_config_to_template(template_config, config)
  # Further check the matches for the right type (storage mode)
  # If modes are different, warn about improper options
  template_config_types <- lapply(default_matches, mode)
  config_types          <- lapply(config_matches, mode)
  type_comparisons <- all.equal(template_config_types, config_types)
  type_compare <- sapply(
    names(config_types),
    \(x) any(grepl(x, type_comparisons)),
    simplify = TRUE
  )
  options_with_bad_types <- names(type_compare)[which(type_compare)]
  if(length(options_with_bad_types) != 0) {
    return(.config_option_type_warn(options_with_bad_types))
  }
  # TODO - what should I return?
  invisible(NULL)

}

# Function to check the config option lengths
.check_config_option_lengths <- function(
  config,
  template_config
) {
  # Subset the config to only include names that are in the template config
  config_matches <-  .align_config_to_template(config, template_config)
  default_matches <- .align_config_to_template(template_config, config)
  # Compare the lengths of the remaining ones
  # This is to specifically check that the vector arguments like 
  # width and interpolate are the right lengths
  compare_lengths <- lengths(default_matches) != lengths(config_matches)
  options_with_bad_lengths <- names(which(compare_lengths))
  #problems_list$length_mismatch <- options_with_bad_lengths
  if(length(options_with_bad_lengths) != 0) {
    return(.config_option_lengths_warn(options_with_bad_lengths))
  }
  # TODO - what should I return?
  invisible(NULL)
}

# Function to subset config to only those that match defaults
.align_config_to_template <- function(
  config,
  template
) {
  # Subset the config to only include names that are in the template config
  both_have_these  <- intersect(names(template), names(config))
  config_matches   <- config[both_have_these]
  return(config_matches)
}

# Some custom warn message handling
warn_config<- function(.subclass, message, call = NULL, ...) {
  warn <- structure(
    list(
      message = message,
      call = call,
      ...
    ),
    class = c(.subclass, "warning", "condition")
  )
  warning(warn$message)
  return(warn)
}

# Config warn for option lengths being wrong
.config_option_lengths_warn <- function(char) {
  message <- c(
    "Warning! ",
    paste(char, collapse = ", "), 
    " do not have the required length in user config. Please check your config\n")
  warn_config(
    "config_option_lengths_warn",
    message = message
  )
}

.config_option_type_warn <- function(char) {
    message <- c(
          "Warning! ",
        paste(char, collapse = ", "), 
        " do not have the required type in user config. Please check your config\n")
  warn_config(
    "config_option_type_warn",
    message = message
  )
}

.config_option_invalid_warn <- function(char) {
    message <- c(
          "Warning! ",
        paste(char, collapse = ", "), 
        " are not valid options in user config. Please check your config\n")
  warn_config(
    "config_option_invalid_warn",
    message = message
  )
}

.config_option_missing_warn <- function(char) {
    message <- c(
          "Warning! ",
        paste(char, collapse = ", "), 
        " are missing from user config. Please check your config\n")
  warn_config(
    "config_option_missing_warn",
    message = message
  )
}
