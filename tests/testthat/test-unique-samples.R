test_that("unique works", {
  expect_length(unique(example_eems), 6)
  expect_length(unique(example_abs), 4)

  dup_abs <- c(example_abs, example_abs)
  class(dup_abs) <- "abslist"
  expect_length(unique(dup_abs), 4)

  dup_eem <- c(example_eems, example_eems)
  class(dup_eem) <- "eemlist"
  expect_length(test <- unique(dup_eem), 6)

})

