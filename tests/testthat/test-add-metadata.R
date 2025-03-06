#testing loaded metadata
  test_that("missing samples are warned", {
  meta <- rbind(metadata, metadata[1,])
  meta$data_identifier[4] <- "missing_sample"

  expect_warning(abs_test <- add_metadata(meta, example_absorbance), "the following sample is in metadata but was missing in data")
  expect_s3_class(abs_test, "abslist")
  expect_equal(length(abs_test), 3)

  meta <- metadata[1,]
  expect_warning(abs_test <- add_metadata(meta, example_absorbance), "the following data are missing from metadata")
  expect_s3_class(abs_test, "abslist")
  expect_equal(length(abs_test), 1)

  meta <- rbind(metadata, metadata[1,])
  meta$data_identifier[4] <- "missing_sample"

  expect_warning(eem_test <- add_metadata(meta, example_eems), "the following sample is in metadata but was missing in data")
  expect_s3_class(eem_test, "eemlist")
  expect_equal(length(eem_test), 6)

  meta <- meta[1,]
  expect_warning(eem_test <- add_metadata(meta, example_eems), "the following data are missing from metadata")
  expect_s3_class(eem_test, "eemlist")
  expect_equal(length(eem_test), 2)
})

  test_that("metadata is added", {
    abs_names <- lapply(add_metadata(metadata, example_absorbance), names)
    expect_length(abs_names[[1]], 11)
    expect_equal(abs_names[[1]], abs_names[[3]])
    expect_equal(abs_names[[1]], c("file","sample", "n", "data","location","meta_name","dilution","analysis_date", "description","doc_mgL","notes"))


    eem_names <- lapply(add_metadata(metadata, example_eems), names)
    expect_length(eem_names[[1]], 14)
    expect_equal(eem_names[[1]], eem_names[[6]])
    expect_equal(eem_names[[1]], c("file","sample", "x", "ex", "em", "location","meta_name","dilution",
                                   "integration_time_s", "raman_area_1s", "analysis_date", "description","doc_mgL","notes"))

    abs_aug <- add_metadata(metadata, example_absorbance)
    eem_aug <- add_metadata(metadata, example_eems)

    expect_equal(get_sample_info(eem_aug, "meta_name"), rep(c("ExampleBlank", "ExampleTeaStd", "ExampleSample"), each=2))
    expect_equal(get_sample_info(eem_aug, "meta_name"), rep(c("ExampleBlank", "ExampleTeaStd", "ExampleSample"), each=2))

  })
