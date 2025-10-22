test_that("correct info is grabbed", {
  #ensure sample names are returned correctly
  expect_equal(get_sample_info(example_eems, "sample"),
               c("B1S1ExampleBlankBEM","B1S1ExampleBlankSEM","B1S2ExampleTeaStdBEM","B1S2ExampleTeaStdSEM",
                 "B1S3ExampleSampleBEM","B1S3ExampleSampleSEM"))
  expect_equal(get_sample_info(example_abs, "sample"),
               c("B1S1ExampleBlankABS","B1S2ExampleTeaStdABS","B1S3ExampleSampleABS"))

  #ensure dates are returned (and in correct format)
  eemlist <- add_metadata(metadata, example_eems)
  expect_equal(get_sample_info(eemlist, "analysis_date"), rep(as.POSIXct("2022-11-14", tz="America/Los_Angeles"), 6))

  abslist <- add_metadata(metadata, example_abs)
  expect_equal(get_sample_info(abslist, "analysis_date"), rep(as.POSIXct("2022-11-14", tz="America/Los_Angeles"), 3))

  #test if getting a single sample
  eemlist <- add_metadata(metadata, example_eems)
  expect_equal(get_sample_info(eemlist[[5]], "doc_mgL"), 1.94)

  abslist <- add_metadata(metadata, example_abs)
  expect_equal(get_sample_info(abslist[[2]], "meta_name"), "ExampleTeaStd")

  #test getting a dataframe for absorbance
  expect_true(is.data.frame(get_sample_info(example_abs, "data")))
  expect_equal(dim(get_sample_info(example_abs, "data")), c(32,4))


  #test getting a matrix
  expect_true(is.matrix(get_sample_info(example_eems, "ex")))
  expect_true(is.list(get_sample_info(example_eems, "x")))
  expect_true(is.matrix(get_sample_info(example_eems, "x")[[1]]))


  #test trying to get info that doesn't exist
  expect_error(get_sample_info(example_eems, "fake_info"), "not found in dataset")
  expect_error(get_sample_info(example_eems[[1]], "fake_info"), "not found in dataset")


})
