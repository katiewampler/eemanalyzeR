test_that("ife corrections are performed", {
  eemlist <- add_metadata(metadata,example_eems)
  abslist <- add_metadata(metadata, example_absorbance)
  correct_eem <- ife_correct(eemlist, abslist)

  #ensure corrections are marked as true
  expect_true(all(sapply(correct_eem, attr, "is_ife_corrected")))

  #ensure eem is returned with all it's things
  expect_equal(length(eemlist)[[1]], length(correct_eem)[[1]])

  #ensure eem is ife corrected
  eem <- example_eems

  ife_vals <- eemR::eem_inner_filter_effect()
})
