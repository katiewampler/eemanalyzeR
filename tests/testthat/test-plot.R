test_that("plotting works", {
# change color scale
  p <- plot(example_processed_eems, pal = c("darkblue", "lightblue"))
  expect_true(is.list(p))
  expect_true(inherits(p[[1]], "ggplot"))

  p <- plot(example_processed_abs, pal = c("darkblue", "lightblue"))
  expect_true(inherits(p, "ggplot"))

#annotation works
  p <- plot(example_processed_eems[[3]], annotate = TRUE)
  expect_true(inherits(p, "ggplot"))

  p <- plot(example_processed_eems[[3]], annotate = TRUE, index_method = "USGS")
  expect_true(inherits(p, "ggplot"))

  p <- plot(example_processed_eems[[3]], annotate = TRUE, index_method = "eemR")
  expect_true(inherits(p, "ggplot"))

  })
