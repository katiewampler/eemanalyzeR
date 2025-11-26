test_that("tea validation works", {
  abslist <- add_metadata(metadata, example_abs)
  check <- validate_std(abslist, system.file("extdata", package = "eemanalyzeR"))
  expect_true(inherits(check, "ggplot"))

  expect_warning(check <- validate_std(abslist[c(1,3)], system.file("extdata", package = "eemanalyzeR")), "No check standard samples found")
  expect_true(is.null(check))

  expect_warning(check <- validate_std(abslist, tempdir()), "Check standard files are missing")
  expect_true(inherits(check, "ggplot"))

})
