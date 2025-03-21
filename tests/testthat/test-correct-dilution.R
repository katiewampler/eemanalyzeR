test_that("dilution corrections are performed", {
  expect_error(correct_dilution(example_eems), "metadata must be added to data to correct samples")

  eemlist <- add_metadata(metadata,example_eems)
  eemlist[[3]]$dilution <- 2
  correct_eem <- correct_dilution(eemlist)

  abslist <- add_metadata(metadata, example_absorbance)
  abslist[[3]]$dilution <- 2
  correct_abs <- correct_dilution(abslist)

  #ensure corrections are marked as true
  expect_true(all(sapply(correct_eem, attr, "is_dil_corrected")))
  expect_true(all(sapply(correct_abs, attr, "is_dil_corrected")))

  expect_s3_class(correct_eem, "eemlist")
  expect_s3_class(correct_abs, "abslist")

  #ensure eem is returned with all it's things
  expect_equal(length(eemlist)[[1]], length(correct_eem)[[1]])
  expect_equal(length(abslist)[[1]], length(correct_abs)[[1]])

  #ensure eem is dilution corrected
  expect_false(.eem_equal(eemlist[[3]]$x, correct_eem[[3]]$x))
  expect_equal(as.vector(eemlist[[3]]$x)*2, as.vector(correct_eem[[3]]$x))

  expect_false(.eem_equal(abslist[[3]]$data, correct_abs[[3]]$data))
  expect_equal(as.vector(abslist[[3]]$data[,2])*2, as.vector(correct_abs[[3]]$data[,2]))

  #ensure correction isn't applied twice
  correct_eem_double <- correct_dilution(correct_dilution(eemlist))
  expect_true(.eem_equal(correct_eem[[3]]$x, correct_eem_double[[3]]$x))

})
