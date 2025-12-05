test_that("plotting works", {

  #check that it returns the right things
  expect_equal(class(plot(example_eems)), "list")
  expect_s3_class(plot(example_eems[[1]]), c("gg", "ggplot"))

  #ensure labels are correct
  plot <- plot(example_eems[[1]])
  expect_equal(plot$labels$fill, "Raw Intensity")

  eem <- example_eems[[1]]
  attr(eem, "is_raman_normalized") <- TRUE
  plot <- plot(eem)
  expect_equal(plot$labels$fill, "Intensity (R.U.)")

  attr(eem, "is_doc_normalized") <- TRUE
  plot <- plot(eem)
  expect_equal(plot$labels$fill, "Intensity (R.U. L mgC \U207B\U00B9)")

  attr(eem, "is_raman_normalized") <- FALSE
  plot <- plot(eem)
  expect_equal(plot$labels$fill, "Intensity (L mgC \U207B\U00B9)")

  #check that error is given if eem is all 0 or NA
    eem_zero <- eem
    eem_zero$x[eem_zero$x != 0] <- 0
    expect_true(is.null(plot(eem_zero)))

    eemlist <- list(eem_zero)
    class(eemlist) <- "eemlist"
    plot(eemlist)

    eem_zero <- eem
    eem_zero$x[eem_zero$x != 0] <- NA
    expect_true(is.null(plot(eem_zero)))

})



