test_that("eem std is calculated", {
  #create temp dir
  test_dir <- withr::local_tempfile()

  #ensure it returns warning with example data
  expect_warning(expect_warning(eem_tea <- create_std(dir = file.path(system.file("extdata", package = "eemanalyzeR"), "long-term-std"),
                                    meta_name="longterm-checkstd-metadata.csv",abs_pattern = "ABS",
                                    type="eem", qaqc_dir = test_dir),
                                "average may be unreliable"), "trimmed EEM's to match absorbance data")


  #check it writes to tempdir
  expect_true(file.exists(file.path(test_dir, "eem-check-std.rds")))

  #read in and make sure it's what we expect
  std <- readRDS(file.path(test_dir, "eem-check-std.rds"))
  expect_equal(length(std), 14)
  expect_equal(get_sample_info(std, "sample"), "long-term-check-std")
  expect_true(inherits(std$x, "matrix"))

})

#same for abs
test_that("abs std is calculated", {
  #create temp dir
  test_dir <- withr::local_tempfile()

  #ensure it returns warning with example data
  expect_warning(abs_tea <- create_std(file.path(dir=system.file("extdata", package = "eemanalyzeR"), "long-term-std"),
                                        meta_name="longterm-checkstd-metadata.csv", abs_pattern = "ABS",
                                        type="abs", qaqc_dir = test_dir))


  #check it writes to tempdir
  expect_true(file.exists(file.path(test_dir, "abs-check-std.rds")))

  #read in and make sure it's what we expect
  std <- readRDS(file.path(test_dir, "abs-check-std.rds"))
  expect_equal(length(std), 11)
  expect_equal(get_sample_info(std, "sample"), "long-term-check-std")
  expect_true(inherits(std$data, "matrix"))

})
