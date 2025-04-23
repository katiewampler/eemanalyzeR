#assuming that the function values are correct

test_that("output is correct",{
  abslist <- add_metadata(metadata, example_absorbance)
  eemlist <- add_metadata(metadata, example_eems)
  indices <- eemR_indices(eemlist, abslist)

  expect_equal(class(indices), "list")
  expect_length(indices, 2)
  expect_equal(class(indices[[1]]), "data.frame")

})
