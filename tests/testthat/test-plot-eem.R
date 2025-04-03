test_that("plotting works", {

  #check that it returns the right things
  expect_equal(class(plot_eem(example_eems)), "list")
  expect_s3_class(plot_eem(example_eems[[1]]), c("gg", "ggplot"))

  #ensure labels are correct
  plot <- plot_eem(example_eems[[1]])
  expect_equal(plot$labels$fill, "Raw Intensity")

  eem <- example_eems[[1]]
  attr(eem, "is_raman_normalized") <- TRUE
  plot <- plot_eem(eem)
  expect_equal(plot$labels$fill, "Intensity (R.U.)")

  attr(eem, "is_doc_normalized") <- TRUE
  plot <- plot_eem(eem)
  expect_equal(plot$labels$fill, "Intensity (R.U. L mgC \U207B\U00B9)")

  attr(eem, "is_raman_normalized") <- FALSE
  plot <- plot_eem(eem)
  expect_equal(plot$labels$fill, "Intensity (L mgC \U207B\U00B9)")

})



