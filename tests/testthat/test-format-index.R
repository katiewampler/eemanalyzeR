test_that("errors are given", {
  ex <- 240:260
  em <- 300:320
  vals <- get_fluorescence(example_eems, ex, em, stat = "max")
  flags <- flag_missing(example_eems, ex=ex, em=em, all=FALSE)

  expect_error(format_index(example_eems, "test_index", vals[-1], flags), "length")
  expect_error(format_index(data.frame(), "test_index", vals, flags), ".is_eemlist")

})

test_that("output is correct", {
  ex <- 240:260
  em <- 300:320
  vals <- get_fluorescence(example_eems, ex, em, stat = "max")
  flags <- flag_missing(example_eems, ex=ex, em=em, all=FALSE)
  index <- format_index(example_eems, "test_index", vals, flags)

  expect_equal(dim(index), c(6,4))
  expect_true(class(index) == "data.frame")
  expect_equal(colnames(index), c("sample_name", "meta_name", "index", "value"))

  eems <- add_metadata(metadata, example_eems)
  index <- format_index(eems, "test_index", vals, flags)

  expect_false(all(index$sample_name == index$meta_name))
})

test_that("flags are carried over", {
  ex <- 240:260
  em <- 300:320
  vals <- get_fluorescence(example_eems, ex, em, stat = "max")
  flags <- flag_missing(example_eems, ex=ex, em=em, all=FALSE)
  index <- format_index(example_eems, "test_index", vals, flags)

  expect_true(all(grepl("DATA_02",index$value)))

})
