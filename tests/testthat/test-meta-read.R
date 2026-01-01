#testing loading metadata via meta_read
test_that("metadata loads", {
  # Test for loading with only directory as input
  expect_s3_class(suppressMessages(meta_read(system.file("extdata", package = "eemanalyzeR"))), "data.frame")
  # Test for loading given full file path
  expect_s3_class(suppressMessages(meta_read(system.file("extdata/metadata_example.csv", package = "eemanalyzeR"))), "data.frame")
  # Test for loading when given meta_file
  expect_s3_class(suppressMessages(meta_read(system.file("extdata", package = "eemanalyzeR"),
                                             meta_file = "metadata_example.csv")),
                  "data.frame")
  # Should error if given the wrong name
  expect_error(suppressMessages(meta_read(system.file("extdata/badexample.csv", package = "eemanalyzeR"))), "Unable to locate metadata")
  expect_error(suppressMessages(meta_read(system.file("extdata", package = "eemanalyzeR"),
                                          meta_file = "badexample.csv")),
                                          "Unable to locate metadata")

  # Should error if two metadata files in directory
  expect_error(suppressMessages(meta_read(test_path("testdata"))), "Multiple possible metadata files in directory.")
  # Specifying filename fixes this error
  expect_s3_class(suppressMessages(meta_read(test_path("testdata"),
                                             meta_file = "metadata_example.csv")), 
                  "data.frame")
})

