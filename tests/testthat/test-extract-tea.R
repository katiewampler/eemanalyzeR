test_that("tea selection works", {
  expect_s3_class(abs_get_tea(example_abs), "abslist")
  expect_length(abs_get_tea(example_abs), 1)
  expect_equal(get_sample_info(abs_get_tea(example_abs), "sample"), "B1S2ExampleTeaStdABS")
  expect_length(abs_get_tea(example_abs, pattern="wrong"), 0)
  expect_s3_class(abs_get_tea(example_abs, pattern="wrong"), "abslist")

})
