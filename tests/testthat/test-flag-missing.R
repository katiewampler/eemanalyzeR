test_that("errors are returned", {
  expect_error(flag_missing(data.frame()), ".is_abs")
  expect_error(flag_missing(example_abs), "is.null")
})

test_that("correct values are given", {
  #checking absorbance data
    expect_equal(flag_missing(example_abs, wl=400), rep(NA, 3)) #data exists
    expect_equal(flag_missing(example_abs, wl=100), rep("DATA01", 3)) #data doesn't exist
    expect_equal(flag_missing(example_abs, wl=100:254, all=FALSE), rep("DATA02", 3)) #some data exists, still calculate

  #checking fluorescence data
    expect_equal(flag_missing(example_eems, ex=270:280, em=300:320), rep(NA, 6)) #data exists
    expect_equal(flag_missing(example_eems, ex=100:150, em=300:320), rep("DATA01", 6)) #data doesn't exist
    expect_equal(flag_missing(example_eems, ex=100:350, em=300:320, all=FALSE), rep("DATA02", 6)) #some data exists, still calculate
})
