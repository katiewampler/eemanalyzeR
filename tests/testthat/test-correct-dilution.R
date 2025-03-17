test_that("dilution corrections are performed", {
  expect_error(correct_dilution(example_eems), "metadata must be added to eemlist to correct samples")

  eemlist <- add_metadata(metadata,example_eems)
  eemlist[[3]]$dilution <- 2
  correct_eem <- correct_dilution(eemlist)

  #ensure corrections are marked as true
  expect_true(all(sapply(correct_eem, attr, "is_dil_corrected")))

  #ensure eem is returned with all it's things
  expect_equal(length(eemlist)[[1]], length(correct_eem)[[1]])

  #ensure eem is raman normalized
  expect_false(.eem_equal(eemlist[[3]]$x, correct_eem[[3]]$x))
  expect_equal(as.vector(eemlist[[3]]$x)*2, as.vector(correct_eem[[3]]$x))

  #ensure correction isn't applied twice
  correct_eem_double <- correct_dilution(correct_dilution(eemlist))
  expect_true(.eem_equal(correct_eem[[3]]$x, correct_eem_double[[3]]$x))

})
