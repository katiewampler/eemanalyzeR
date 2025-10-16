test_that("eem mdl is calculated", {
  #create temp dir
  test_dir <- withr::local_tempfile()

  #ensure it returns warning with example data
    expect_warning(eem_mdl <- get_mdl(file.path(system.file("extdata", package = "eemanalyzeR"), "long-term-blanks"),
                       meta_name="longtermblank-metadata.csv", pattern = "longtermblank",
                       type="eem", output_dir = test_dir))


  #check it writes to tempdir
    expect_true(file.exists(file.path(test_dir, "eem-mdl.Rds")))

  #read in and make sure it's what we expect
    mdl <- readRDS(file.path(test_dir, "eem-mdl.Rds"))
    expect_equal(length(mdl), 14)
    expect_equal(get_sample_info(mdl, "sample"), "long-term-mdl")
    expect_true(inherits(mdl$x, "matrix"))

})

#same for abs
test_that("abs mdl is calculated", {
  #create temp dir
  test_dir <- withr::local_tempfile()

  #ensure it returns warning with example data
  expect_warning(eem_mdl <- get_mdl(file.path(system.file("extdata", package = "eemanalyzeR"), "long-term-blanks"),
                                    meta_name="longtermblank-metadata.csv", pattern = "longtermblank",
                                    type="abs", output_dir = test_dir))


  #check it writes to tempdir
  expect_true(file.exists(file.path(test_dir, "abs-mdl.Rds")))

  #read in and make sure it's what we expect
  mdl <- readRDS(file.path(test_dir, "abs-mdl.Rds"))
  expect_equal(length(mdl), 11)
  expect_equal(get_sample_info(mdl, "sample"), "long-term-mdl")
  expect_true(inherits(mdl$data, "matrix"))

})
