test_that("raman normalizations are performed", {
  expect_error(raman_normalize(example_eems), "metadata must be added to eemlist and abslist to link samples")

  eemlist <- add_metadata(metadata,example_eems)
  correct_eem <- raman_normalize(eemlist)

  #ensure corrections are marked as true
  expect_true(all(sapply(correct_eem, attr, "is_raman_normalized")))

  #ensure eem is returned with all it's things
  expect_equal(length(eemlist)[[1]], length(correct_eem)[[1]])

  #ensure eem is raman normalized
  expect_false(.eem_equal(eemlist[[3]]$x, correct_eem[[3]]$x))
  expect_equal(as.vector(eemlist[[3]]$x)/(eemlist[[3]]$raman_area_1s*eemlist[[3]]$integration_time_s), as.vector(correct_eem[[3]]$x))
})
