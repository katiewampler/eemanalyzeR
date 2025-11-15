test_that("eem tea std is calculated", {
  #create temp dir
  test_dir <- withr::local_tempfile()

  #ensure it returns warning with example data
  expect_warning(expect_warning(eem_tea <- create_tea_std(file.path(system.file("extdata", package = "eemanalyzeR"), "long-term-tea"),
                                    meta_name="longtermteastd-metadata.csv", pattern = "longterm-tea",
                                    type="eem", output_dir = test_dir),
                                "average may be unreliable"), "trimmed EEM's to match absorbance data")


  #check it writes to tempdir
  expect_true(file.exists(file.path(test_dir, "eem-tea-std.Rds")))

  #read in and make sure it's what we expect
  tea <- readRDS(file.path(test_dir, "eem-tea-std.Rds"))
  expect_equal(length(tea), 14)
  expect_equal(get_sample_info(tea, "sample"), "long-term-tea-std")
  expect_true(inherits(tea$x, "matrix"))

})

#same for abs
test_that("abs tea is calculated", {
  #create temp dir
  test_dir <- withr::local_tempfile()

  #ensure it returns warning with example data
  expect_warning(abs_tea <- create_tea_std(file.path(system.file("extdata", package = "eemanalyzeR"), "long-term-tea"),
                                        meta_name="longtermteastd-metadata.csv", pattern = "longterm-tea",
                                        type="abs", output_dir = test_dir))


  #check it writes to tempdir
  expect_true(file.exists(file.path(test_dir, "abs-tea-std.Rds")))

  #read in and make sure it's what we expect
  tea <- readRDS(file.path(test_dir, "abs-tea-std.Rds"))
  expect_equal(length(tea), 11)
  expect_equal(get_sample_info(tea, "sample"), "long-term-tea-std")
  expect_true(inherits(tea$data, "matrix"))

})
