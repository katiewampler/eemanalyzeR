test_that("errors are returned", {
  expect_error(get_abs_slope(example_eems), ".is_abs")
  expect_error(get_abs_slope(example_absorbance, lim="not_number"), "is.numeric")
  expect_error(get_abs_slope(example_absorbance, lim=250), "length")
  expect_error(get_abs_slope(example_absorbance, lim=c(250,350), cuvle = "not_number"), "is.numeric")
})

test_that("correct values are returned", {
  abslist <- add_metadata(metadata, example_absorbance)
  expect_equal(get_abs_slope(abslist, lim=c(250,350)), c(0.04412657, 0.01325616, 0.01361086), tolerance = 1e-4)
})
