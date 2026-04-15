# qaqc creation:
  #if qaqc is specified, creates files there, names files the method name
  #if qaqc if NA
    #asks if it can update directory to default
    #if yes -> updates qaqc_dir in user config and .pkgenv to default rappdirs::user_data_dir(appname = "eemanalyzeR"), "qaqc-stds"
    #if no leaves as NA -> returns object

# Testing std creation when dir is specified
test_that("std are calculated and saved when dir is specified", {
  #create temp dir
  test_dir <- withr::local_tempfile()

  #ensure it returns warning with example data
  expect_warning(eem_std <- create_std(dir = file.path(system.file("extdata", package = "eemanalyzeR"), "long-term-std"),
                                       meta_name="longterm-checkstd-metadata.csv",abs_pattern = "ABS",
                                       type="eem", qaqc_dir = test_dir), "average may be unreliable")

  #ensure it returns warning with example data
  expect_warning(abs_std <- create_std(file.path(dir=system.file("extdata", package = "eemanalyzeR"), "long-term-std"),
                                       meta_name="longterm-checkstd-metadata.csv", abs_pattern = "ABS",
                                       type="abs", qaqc_dir = test_dir), "average may be unreliable")


  #check it writes to tempdir
  expect_true(file.exists(file.path(test_dir,  "default/default-eem-check-std.rds")))
  expect_true(file.exists(file.path(test_dir, "default/default-abs-check-std.rds")))

  #read in and make sure it's what we expect
    std <- readRDS(file.path(test_dir, "default/default-eem-check-std.rds"))
    expect_equal(length(std), 14)
    expect_equal(get_sample_info(std, "sample"), "long-term-check-std")
    expect_true(inherits(std$x, "matrix"))

  #read in and make sure it's what we expect
    std <- readRDS(file.path(test_dir, "default/default-abs-check-std.rds"))
    expect_equal(length(std), 11)
    expect_equal(get_sample_info(std, "sample"), "long-term-check-std")
    expect_true(inherits(std$data, "matrix"))
})

# Testing std creation when qaqc is NA
test_that("std are exported when dir is NA and not updated", {
  #ensure it returns warning with example data
  expect_warning(std <- create_std(dir = file.path(system.file("extdata", package = "eemanalyzeR"), "long-term-std"),
                                       meta_name="longterm-checkstd-metadata.csv", abs_pattern = "ABS",
                                       type="eem", qaqc_dir = NA, update_config=FALSE), "average may be unreliable")

  #make sure it's returned and it's what we expect
  expect_equal(length(std), 14)
  expect_equal(get_sample_info(std, "sample"), "long-term-check-std")
  expect_true(inherits(std$x, "matrix"))

})

# Testing MDL creation when qaqc is NA and user wants to update
test_that("std are exported when dir is NA and updated",{
  dummy_dir <- withr::local_tempfile()
  with_mocked_bindings(
    .user_data_dir = function() dummy_dir,
    {
      #setting qaqc_dir to NA while creating should trigger asking to fill
      #ensure it returns warning with example data
      #ensure it returns warning with example data
      expect_warning(eem_std <- create_std(dir = file.path(system.file("extdata", package = "eemanalyzeR"), "long-term-std"),
                                           meta_name="longterm-checkstd-metadata.csv",abs_pattern = "ABS",
                                           method = "testthat-checks",
                                           type="eem", qaqc_dir = NA), "average may be unreliable")

      #ensure it returns warning with example data
      expect_warning(abs_std <- create_std(file.path(dir=system.file("extdata", package = "eemanalyzeR"), "long-term-std"),
                                           meta_name="longterm-checkstd-metadata.csv", abs_pattern = "ABS",
                                           method = "testthat-checks",
                                           type="abs", qaqc_dir = NA), "average may be unreliable")

    })

  #check it writes to local
  expect_true(file.exists(file.path(dummy_dir, "qaqc-stds", "testthat-checks", "testthat-checks-eem-check-std.rds")))
  expect_true(file.exists(file.path(dummy_dir, "qaqc-stds", "testthat-checks", "testthat-checks-abs-check-std.rds")))

  #check that qaqc dir is updated
  expect_equal(.pkgenv$config$qaqc_dir, normalizePath(file.path(dummy_dir, "qaqc-stds"), winslash = "/"))

  config <- yaml::read_yaml(file.path(dummy_dir, "user-config.yaml"))
  expect_equal(config$qaqc_dir,normalizePath(file.path(dummy_dir, "qaqc-stds"), winslash = "/") )
  expect_equal(.pkgenv$config$qaqc_dir, normalizePath(file.path(dummy_dir, "qaqc-stds"), winslash = "/"))

})
