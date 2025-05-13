test_that("correct flags are returned", {
  expect_equal(get_ratios(1,0), "DATA03")
  expect_equal(get_ratios(1,NA), "DATA01")

  expect_equal(get_ratios(6,3), '2')


  expect_equal(get_ratios(c(1,1,1),c(0,0,0)), rep("DATA03",3))
  expect_equal(get_ratios(c(1,1,1),c(NA,NA,NA)), rep("DATA01",3))

  expect_equal(get_ratios(c(6,12,18),c(3,3,3)), c('2','4','6'))
})

test_that("errors are given", {
  expect_error(get_ratios(c(1,2,4), c(1,3)), "length")
  expect_no_error(get_ratios(c(1,2,4), 1))
})
