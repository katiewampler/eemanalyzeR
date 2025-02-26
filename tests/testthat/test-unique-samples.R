test_that("unique works", {
  expect_length(unique(example_eems), 4)
  expect_length(unique(example_absorbance), 3)

  dup_abs <- c(example_absorbance, example_absorbance)
  class(dup_abs) <- "abslist"
  expect_length(unique(dup_abs), 3)

  dup_eem <- c(example_eems, example_eems)
  class(dup_eem) <- "eemlist"
  expect_length(test <- unique(dup_eem), 4)

})

