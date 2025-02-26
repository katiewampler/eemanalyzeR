test_that("tea selection works", {
  expect_s3_class(abs_get_tea(example_absorbance), "abslist")
  expect_length(abs_get_tea(example_absorbance), 1)
  expect_equal(get_sample_info(abs_get_tea(example_absorbance), "sample"), "B1S2ExampleTeaStdABS")
  expect_length(abs_get_tea(example_absorbance, pattern="wrong"), 0)
  expect_s3_class(abs_get_tea(example_absorbance, pattern="wrong"), "abslist")

})
