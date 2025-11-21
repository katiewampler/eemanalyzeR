test_that("sample selection works", {
  #add metadata to get types
  abs <- add_metadata(metadata, example_abs)
  eem <- add_metadata(metadata, example_eems)

  #get tea absorbance
    tea <- subset_type(abs, "check")
    expect_equal(get_sample_info(tea, "sample"), c("B1S2ExampleTeaStdABS", "ManualExampleTeaAbsSpectraGraphs"))

  #get blank eems
    blk <- subset_type(eem, "iblank")
    expect_equal(get_sample_info(blk, "sample"), c("B1S1ExampleBlankBEM","B1S2ExampleTeaStdBEM","B1S3ExampleSampleBEM", "ManualExampleTeaWaterfallPlotBlank"))

  #use negate = TRUE to get non tea samples
    nontea <- subset_type(eem, "check", negate=TRUE)
    expect_equal(get_sample_info(nontea, "sample"),
                 c("B1S1ExampleBlankBEM", "B1S1ExampleBlankSEM","B1S2ExampleTeaStdBEM","B1S3ExampleSampleBEM", "B1S3ExampleSampleSEM", "ManualExampleTeaWaterfallPlotBlank"))

  #check that multiple doesn't fail
    blk <- subset_type(eem, c("iblank", "sblank"))
    expect_equal(get_sample_info(blk, "sample"), c("B1S1ExampleBlankBEM", "B1S1ExampleBlankSEM",  "B1S2ExampleTeaStdBEM", "B1S3ExampleSampleBEM", "ManualExampleTeaWaterfallPlotBlank"))
})
