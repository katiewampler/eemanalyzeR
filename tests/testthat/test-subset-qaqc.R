test_that("qaqc sample selection works", {
  #get tea absorbance
    tea <- subset_qaqc(example_abs, "tea_std")
    expect_equal(get_sample_info(tea, "sample"), "B1S2ExampleTeaStdABS")

  #get blank eems
    blk <- subset_qaqc(example_eems)
    expect_equal(get_sample_info(blk, "sample"), c("B1S1ExampleBlankBEM","B1S2ExampleTeaStdBEM","B1S3ExampleSampleBEM"))

  #use negate = TRUE to get non qaqc samples
    nonblk <- subset_qaqc(example_eems, negate=TRUE)
    expect_equal(get_sample_info(nonblk, "sample"), c("B1S1ExampleBlankSEM","B1S2ExampleTeaStdSEM","B1S3ExampleSampleSEM"))

})
