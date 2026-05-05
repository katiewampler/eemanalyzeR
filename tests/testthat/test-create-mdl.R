# qaqc creation:
  #if qaqc is specified, creates files there, names files the method name
  #if qaqc if NA
    #asks if it can update directory to default
      #if yes -> updates qaqc_dir in user config and .pkgenv to default rappdirs::user_data_dir(appname = "eemanalyzeR"), "qaqc-stds"
      #if no leaves as NA -> returns object


# Testing MDL creation when dir is specified
test_that("mdls are calculated and saved when dir is specified", {
  #create temp dir
  test_dir <- withr::local_tempfile()

  #ensure it returns warning with example data
    expect_warning(eem_mdl <- create_mdl(dir=file.path(system.file("extdata", package = "eemanalyzeR"), "long-term-blanks"),
                       meta_name="longtermblank-metadata.csv",
                       type="eem", qaqc_dir = test_dir), "Calculating MDL based on less than 20 samples")

    #ensure it returns warning with example data
    expect_warning(abs_mdl <- create_mdl(dir=file.path(system.file("extdata", package = "eemanalyzeR"), "long-term-blanks"),
                                         meta_name="longtermblank-metadata.csv",
                                         type="abs", qaqc_dir =test_dir))


  #check it writes to tempdir
    expect_true(file.exists(file.path(test_dir,  "default/default-eem-mdl.rds")))
    expect_true(file.exists(file.path(test_dir, "default/default-abs-mdl.rds")))

  #read in and make sure it's what we expect
    mdl <- readRDS(file.path(test_dir, "default/default-eem-mdl.rds"))
    expect_equal(length(mdl), 14)
    expect_equal(get_sample_info(mdl, "sample"), "long-term-mdl")
    expect_true(inherits(mdl$x, "matrix"))

  #read in and make sure it's what we expect
    mdl <- readRDS(file.path(test_dir, "default/default-abs-mdl.rds"))
    expect_equal(length(mdl), 11)
    expect_equal(get_sample_info(mdl, "sample"), "long-term-mdl")
    expect_true(inherits(mdl$data, "matrix"))
})

# Testing MDL creation when qaqc is NA
test_that("mdls are exported when dir is NA and not updated", {
    #ensure it returns warning with example data
    expect_warning(eem_mdl <- create_mdl(dir=file.path(system.file("extdata", package = "eemanalyzeR"), "long-term-blanks"),
                                         meta_name="longtermblank-metadata.csv",
                                         type="eem", qaqc_dir = NA, update_config=FALSE), "Calculating MDL based on less than 20 samples")

    #make sure it's returned and it's what we expect
    expect_equal(length(eem_mdl), 14)
    expect_equal(get_sample_info(eem_mdl, "sample"), "long-term-mdl")
    expect_true(inherits(eem_mdl$x, "matrix"))

})

# Testing MDL creation when qaqc is NA and user wants to update
test_that("mdls are exported when dir is NA and updated",{
  dummy_dir <- withr::local_tempdir()
  dummy_config_path <- file.path(dummy_dir, "user-config.yaml")
  file.copy(
    file.path(system.file("extdata", package = "eemanalyzeR"),"eemanalyzeR-config.yaml"),
    dummy_config_path)
  with_mocked_bindings(
    code = {
      #setting qaqc_dir to NA while creating should trigger asking to fill
      #ensure it returns warning with example data
      expect_warning(eem_mdl <- create_mdl(dir=file.path(system.file("extdata", package = "eemanalyzeR"), "long-term-blanks"),
                                           method = "testthat-checks",
                                           meta_name="longtermblank-metadata.csv",
                                           type="eem", qaqc_dir = NA),"Calculating MDL based on less than 20 samples")

      expect_warning(abs_mdl <- create_mdl(dir=file.path(system.file("extdata", package = "eemanalyzeR"), "long-term-blanks"),
                                           method = "testthat-checks",
                                           meta_name="longtermblank-metadata.csv",
                                           type="abs", qaqc_dir = NA),"Calculating MDL based on less than 20 samples")
    },
    .user_data_dir = function() dummy_dir,
    .user_config_path = function() dummy_config_path,
    )

  #check it writes to local
  expect_true(file.exists(file.path(dummy_dir, "qaqc-stds", "testthat-checks", "testthat-checks-eem-mdl.rds")))
  expect_true(file.exists(file.path(dummy_dir, "qaqc-stds", "testthat-checks", "testthat-checks-abs-mdl.rds")))

  #check that qaqc dir is updated
  expect_equal(.pkgenv$config$qaqc_dir, normalizePath(file.path(dummy_dir, "qaqc-stds"), winslash = "/"))

  config <- yaml::read_yaml(file.path(dummy_dir, "user-config.yaml"))
  expect_equal(config$qaqc_dir,normalizePath(file.path(dummy_dir, "qaqc-stds"), winslash = "/") )
  expect_equal(.pkgenv$config$qaqc_dir, normalizePath(file.path(dummy_dir, "qaqc-stds"), winslash = "/"))

})
