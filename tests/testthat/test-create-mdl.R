# Testing MDL creation for EEMs 
test_that("eem mdl is calculated and saved", {
  #create temp dir
  test_dir <- withr::local_tempfile()

  #ensure it returns warning with example data
    expect_warning(eem_mdl <- create_mdl(dir=file.path(system.file("extdata", package = "eemanalyzeR"), "long-term-blanks"),
                       meta_name="longtermblank-metadata.csv",
                       type="eem", qaqc_dir = test_dir), "Calculating MDL based on less than 20 samples")


  #check it writes to tempdir
    expect_true(file.exists(file.path(test_dir, "eem-mdl.rds")))

  #read in and make sure it's what we expect
    mdl <- readRDS(file.path(test_dir, "eem-mdl.rds"))
    expect_equal(length(mdl), 14)
    expect_equal(get_sample_info(mdl, "sample"), "long-term-mdl")
    expect_true(inherits(mdl$x, "matrix"))

})

test_that("eem mdl is calculated and exported to global environment when qaqc_dir = NA", {
  #ensure it returns warning with example data
    expect_warning(eem_mdl <- create_mdl(dir=file.path(system.file("extdata", package = "eemanalyzeR"), "long-term-blanks"),
                       meta_name="longtermblank-metadata.csv",
                       type="eem", qaqc_dir = NA), "Calculating MDL based on less than 20 samples")

  #read in and make sure it's what we expect
    expect_equal(length(eem_mdl), 14)
    expect_equal(get_sample_info(eem_mdl, "sample"), "long-term-mdl")
    expect_true(inherits(eem_mdl$x, "matrix"))
})

# Testing MDL creation for ABS
test_that("abs mdl is calculated", {
  #create temp dir
  test_dir <- withr::local_tempfile()

  #ensure it returns warning with example data
  expect_warning(eem_mdl <- create_mdl(dir=file.path(system.file("extdata", package = "eemanalyzeR"), "long-term-blanks"),
                                    meta_name="longtermblank-metadata.csv",
                                    type="abs", qaqc_dir =test_dir))


  #check it writes to tempdir
  expect_true(file.exists(file.path(test_dir, "abs-mdl.rds")))

  #read in and make sure it's what we expect
  mdl <- readRDS(file.path(test_dir, "abs-mdl.rds"))
  expect_equal(length(mdl), 11)
  expect_equal(get_sample_info(mdl, "sample"), "long-term-mdl")
  expect_true(inherits(mdl$data, "matrix"))

})

test_that("abs mdl is calculated and exported to global environment when qaqc_dir = NA", {
  #ensure it returns warning with example data
    expect_warning(abs_mdl <- create_mdl(dir=file.path(system.file("extdata", package = "eemanalyzeR"), "long-term-blanks"),
                       meta_name="longtermblank-metadata.csv",
                       type="abs", qaqc_dir = NA), "Calculating MDL based on less than 20 samples")

  #read in and make sure it's what we expect
    expect_equal(length(abs_mdl), 11)
    expect_equal(get_sample_info(abs_mdl, "sample"), "long-term-mdl")
    expect_true(inherits(abs_mdl$data, "matrix"))
})
