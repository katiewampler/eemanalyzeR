#testing loading metadata via meta_read
test_that("metadata loads", {
  # Test for loading with only directory as input
  expect_s3_class(suppressMessages(meta_read(system.file("extdata", package = "eemanalyzeR"))), "data.frame")
  # Test for loading given full file path
  expect_s3_class(suppressMessages(meta_read(system.file("extdata/metadata_example.csv", package = "eemanalyzeR"))), "data.frame")
  # Should error if given the wrong name
  expect_error(suppressMessages(meta_read(system.file("extdata/badexample.csv", package = "eemanalyzeR"))), "unable to locate metadata")
})
