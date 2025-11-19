test_that("samples are given the right attributes", {
  #should return exactly the same thing
    eemlist <- mark_qaqc(example_eems)
    expect_equal(eemlist, example_eems)

  #check correct samples are marked
    eemlist <- mark_qaqc(example_eems, blk_pattern ="BEM", tea_pattern="Tea")

    expect_equal(sapply(eemlist, function(x){attr(x, "is_blank")}), c(TRUE,FALSE,TRUE,FALSE,TRUE,FALSE))
    expect_equal(sapply(eemlist, function(x){attr(x, "is_check_std")}), c(FALSE,FALSE,TRUE,TRUE,FALSE,FALSE))

})
