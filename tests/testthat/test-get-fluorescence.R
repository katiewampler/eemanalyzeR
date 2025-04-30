test_that("errors are returned", {
  expect_error(get_fluorescence(example_absorbance), ".is_eem")
  expect_error(get_fluorescence(example_eems, ex="not_number"), "is.numeric")
  expect_error(get_fluorescence(example_eems, ex=254, em="not_number"), "is.numeric")
  expect_error(get_fluorescence(example_eems, ex=254, em=400, stat="wrong"), "stat")
})

test_that("correct values are returned", {
  expect_equal(as.numeric(get_fluorescence(example_eems, ex=250:260, em=380:480)),
               c(2918.078,5585.938,2918.078,14917.394,2918.078,5732.431), tolerance=1e-3)

  expect_equal(as.numeric(get_fluorescence(example_eems, ex=390, em=509)), c(11.477041,7.143219,11.477041,153.453184,11.477041,716.439407))

  expect_equal(as.numeric(get_fluorescence(example_eems, ex=250:260, em=380:480, stat="sum")),
               c(148781.0,293416.9,148781.0,1786567.5,148781.0,2490878.2), tolerance=1e-2)

  expect_equal(get_fluorescence(example_eems, ex=100, em=100), rep("DATA01", 6))
})
