test_that("processing works", {
  eemlist <- add_metadata(metadata,example_eems)
  abslist <- add_metadata(metadata, example_abs)
  blanklist <- subset_type(eemlist, "iblank")
  eemlist <- add_blanks(eemlist, blanklist)


  correct_eem <- process_eem(eemlist, abslist)
  expect_s3_class(correct_eem, "eemlist")

  #check clipping
  expect_equal(length(correct_eem[[1]]$em), 26)
  expect_equal(length(correct_eem[[1]]$ex), 11)

})
