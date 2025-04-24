test_that("function selector works",{
  expect_equal(class(get_indices_function()), "function")
  expect_equal(class(get_indices_function("eemR")), "function")
  expect_equal(class(get_indices_function("usgs")), "function")
  expect_error(get_indices_function("custom"), "custom is not a known function to generate indices")

  test_func <- function(x){x <- x + 1}
  expect_equal(class(get_indices_function(test_func)), "function")
})
