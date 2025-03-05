test_that("ife corrections are performed", {
  eemlist <- add_metadata(metadata,example_eems)
  abslist <- add_metadata(metadata, example_absorbance)
  expect_warning(correct_eem <- ife_correct(eemlist, abslist), "trimmed EEM's to match absorbance data wavelengths")

  #ensure corrections are marked as true
  expect_true(all(sapply(correct_eem, attr, "is_ife_corrected")))

  #ensure eem is returned with all it's things
  expect_equal(length(eemlist)[[1]], length(correct_eem)[[1]])
  #ensure eem is ife corrected
  eemlist <- eemR::eem_cut(eemlist, 801:1000, 801:1000, exact=F) #cut to same as ife
  expect_false(.eem_equal(eemlist[[3]]$x, correct_eem[[3]]$x))
})
