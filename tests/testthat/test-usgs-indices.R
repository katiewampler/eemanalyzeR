#assuming that the function values are correct

test_that("output is correct",{
  abslist <- example_processed_abs
  eemlist <- example_processed_eems
  qaqc_dir <- system.file("extdata", package = "eemanalyzeR")
  indices <- usgs_indices(eemlist, abslist, qaqc_dir = qaqc_dir)

  expect_equal(class(indices), "list")
  expect_length(indices, 2)
  expect_equal(class(indices[[1]]), "data.frame")

  #check correct indices are returned
  expect_true("FDOM_52902" %in% indices$eem_index$index)
  expect_true("SUVA254_63162" %in% indices$abs_index$index)

})
