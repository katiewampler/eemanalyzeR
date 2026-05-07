# Test detecting user configuration errors
test_that(
  "Configuration utilities can detect problems with the config",
  {
    # Make a test config with 4 problems
    template_config <- yaml::read_yaml(file.path(
      system.file("extdata", package = "eemanalyzeR"),
      "eemanalyzeR-config.yaml"
    ))

    # Test config code
    t_config <- template_config
    # Change mode
    t_config$eem_import_func <- TRUE
    # Change text
    t_config$abs_file_ext <- "csv"
    # Change number
    t_config$tolerance <- 0.3
    # Change bool
    t_config$sum_plot <- 4
    # Change 1 value in list/vector
    t_config$ex_clip[1] <- 249
    t_config$eem_skip <- NULL
    # Make up option
    t_config$eemm_skip <- "test"
    # Change length of list option
    t_config$width <- c(1, 2, 3)

    problems <- .validate_config(t_config, template_config)
    # Test that we can detect invalid user config options
    expect_s3_class(problems$invalid_options, "config_option_invalid_warn")
    # Test that we can detect missing user config options
    expect_s3_class(problems$missing_options, "config_option_missing_warn")
    # Test that we can detect invalid user config option types
    expect_s3_class(problems$invalid_types,   "config_option_type_warn")
    # Test that we can detect invalid user config option lengths
    expect_s3_class(problems$invalid_lengths, "config_option_lengths_warn")
    
  }

)

# Test loading the default user config
test_that("User Config can be correctly loaded",
  {
  # Set up testing environment
  dummy_dir <- withr::local_tempdir()
  dummy_config_path <- file.path(dummy_dir, "user-config.yaml")
  file.copy(
    file.path(system.file("extdata", package = "eemanalyzeR"),"eemanalyzeR-config.yaml"),
    dummy_config_path)
  template_config <- yaml::read_yaml(dummy_config_path)
  # Create a bad user config
  badmode_config <- template_config
  badmode_config$eem_import_func <- TRUE
  yaml::write_yaml(badmode_config, 
    file = file.path(dummy_dir, "badmode_config.yaml",
    handlers = list(logical = yaml::verbatim_logical))
  )
  # Remove an option
  missingoption_config <- template_config
  missingoption_config$eem_skip <- NULL
  yaml::write_yaml(missingoption_config, 
    file = file.path(dummy_dir, "missingoption_config.yaml"),
    handlers = list(logical = yaml::verbatim_logical)
  )
  # Invalid option
  invalidoption_config <- template_config
  invalidoption_config$eemm_skip <- "test"
  yaml::write_yaml(invalidoption_config, 
    file = file.path(dummy_dir, "invalidoption_config.yaml"),
    handlers = list(logical = yaml::verbatim_logical)
  )
  # Change length of list option
  badlength_config <- template_config
  badlength_config$width <- c(1, 2, 3)
  yaml::write_yaml(badlength_config, 
    file = file.path(dummy_dir, "badlength_config.yaml"),
    handlers = list(logical = yaml::verbatim_logical)
  )

  with_mocked_bindings(
    code = {
      # Error in loading badmode_config
      expect_error(load_user_config(file.path(dummy_dir, "badmode_config.yaml")))
      # Error in loading missingoption_config
      expect_error(load_user_config(file.path(dummy_dir, "missingoption_config.yaml")))
      # Error in loading invalidoption_config
      expect_error(load_user_config(file.path(dummy_dir, "invalidoption_config.yaml")))
      # Error in loading badlength_config
      expect_error(load_user_config(file.path(dummy_dir, "badlength_config.yaml")))

      # Loading the config correctly modifies the session config
      modify_session_config(abs_file_ext = "testtext")

      # No error in the template config
      expect_no_error(load_user_config())
      expect_equal(list_session_config()$abs_file_ext,
                   template_config$abs_file_ext)

        
    },
    .user_data_dir = function() dummy_dir,
    .user_config_path = function() dummy_config_path,
    )

  })

