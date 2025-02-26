test_that("blank removal works", {
  expect_length(samp <- eem_rm_blank(example_eems, pattern = "BEM"),3)
  expect_equal(get_sample_info(samp, "sample"), c("B1S1ExampleBlankSEM","B1S2ExampleTeaStdSEM","B1S3ExampleSampleSEM"))
  expect_length(eem_rm_blank(example_eems, pattern = "wrong"),6)
  expect_s3_class(eem_rm_blank(example_eems, pattern = "wrong"),"eemlist")

})

test_that("blank selection works",{

  expect_length(blk <- eem_get_blank(example_eems, "BEM"), 3)
  expect_equal(get_sample_info(blk, "sample"), c("B1S1ExampleBlankBEM","B1S2ExampleTeaStdBEM","B1S3ExampleSampleBEM"))
  expect_length(eem_get_blank(example_eems, pattern = "wrong"),0)
  expect_s3_class(eem_get_blank(example_eems, pattern = "wrong"),"eemlist")

})

