test_that("samples are removed", {
  names <- get_sample_info(example_eems, "sample")
  eem_subset <- subset_samples(example_eems, "sample", names[1], verbose = F)
  expect_length(eem_subset, 5)
  expect_equal(get_sample_info(eem_subset, "sample"),
               c("B1S1ExampleBlankSEM","B1S2ExampleTeaStdBEM","B1S2ExampleTeaStdSEM",
                 "B1S3ExampleSampleBEM","B1S3ExampleSampleSEM"))
})

test_that("samples are kept", {
  eemlist <- add_metadata(metadata, example_eems)
  names <- get_sample_info(eemlist, "meta_name")
  eem_subset <- subset_samples(eemlist, "meta_name", names[1], keep=T, verbose = F)
  expect_length(eem_subset, 2)
  expect_equal(get_sample_info(eem_subset, "sample"),
               c("B1S1ExampleBlankBEM","B1S1ExampleBlankSEM"))
})

test_that("messages are provided", {
  expect_message(eem_subset <- subset_samples(example_eems, "sample", "doesn't exist"), "Nothing to remove.")
  expect_length(eem_subset, 6)
  expect_equal(get_sample_info(eem_subset, "sample"),
               c("B1S1ExampleBlankBEM","B1S1ExampleBlankSEM","B1S2ExampleTeaStdBEM","B1S2ExampleTeaStdSEM",
                 "B1S3ExampleSampleBEM","B1S3ExampleSampleSEM"))

  names <- get_sample_info(example_eems, "sample")
  expect_message(eem_subset <- subset_samples(example_eems, "sample", names[1], keep=F), "Removed sample")
  expect_message(eem_subset <- subset_samples(example_eems, "sample", names[1], keep=T), "Extracted sample")


})
