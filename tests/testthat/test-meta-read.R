#testing loading metadata
test_that("metadata loads", {
  expect_s3_class(meta_read(system.file("extdata", package = "eemanalyzeR")), "data.frame")
  expect_error(meta_read(system.file("extdata", package = "eemanalyzeR"), name="wrong_name"), "unable to locate metadata")
})
