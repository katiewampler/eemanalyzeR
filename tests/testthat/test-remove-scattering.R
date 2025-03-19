test_that("scattering is removed", {
  eemlist <- example_eems
  correct_eem <- remove_scattering(eemlist,cores=1)

  #ensure corrections are marked as true
  expect_true(all(sapply(correct_eem, attr, "is_scatter_corrected")))

  #ensure eem is returned with all it's things
  expect_equal(length(eemlist)[[1]], length(correct_eem)[[1]])

  #ensure eem has scattering removed
  expect_false(.eem_equal(eemlist[[3]]$x, correct_eem[[3]]$x))
  expect_true(any(is.na(correct_eem[[3]]$x)))

  expect_s3_class(correct_eem, "eemlist")
})
