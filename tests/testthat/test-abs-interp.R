test_that("absorbance gets interpolated", {

  abslist <- abs_interp(example_abs)

  expect_equal(get_sample_info(abslist, "n"), c(559, 559,559))
  expect_equal(dim(get_sample_info(abslist, "data")), c(559,4))

  expect_equal(class(abslist), "abslist")
})

test_that("errors are given", {

  expect_error(abs_interp(example_abs, type="wrong"), "type %in%")
  expect_error(abs_interp(example_eems, type="wrong"), ".is_abs")

})


test_that("different interpolation methods work", {

  abslist1 <- abs_interp(example_abs[[1]], type="spline")
  abslist2 <- abs_interp(example_abs[[1]], type="linear")

  expect_false(all(abslist1$data[,2] == abslist2$data[,2]))
})
