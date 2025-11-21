#testing loaded metadata
  test_that("missing samples are warned", {
  meta <- rbind(metadata, metadata[1,])
  meta$data_identifier[5] <- "missing_sample"

  expect_warning(abs_test <- add_metadata(meta, example_abs), "the following sample is in metadata but was missing in data")
  expect_s3_class(abs_test, "abslist")
  expect_equal(length(abs_test), 4)

  meta <- metadata[1,]
  expect_warning(abs_test <- add_metadata(meta, example_abs), "the following data are missing from metadata")
  expect_s3_class(abs_test, "abslist")
  expect_equal(length(abs_test), 1)

  meta <- rbind(metadata, metadata[1,])
  meta$data_identifier[5] <- "missing_sample"

  expect_warning(eem_test <- add_metadata(meta, example_eems), "the following sample is in metadata but was missing in data")
  expect_s3_class(eem_test, "eemlist")
  expect_equal(length(eem_test), 8)

  meta <- meta[1,]
  expect_warning(eem_test <- add_metadata(meta, example_eems), "the following data are missing from metadata")
  expect_s3_class(eem_test, "eemlist")
  expect_equal(length(eem_test), 2)
})

  test_that("metadata is added", {
    abs_names <- lapply(add_metadata(metadata, example_abs), names)
    expect_length(abs_names[[1]], 11)
    expect_equal(abs_names[[1]], abs_names[[3]])
    expect_equal(abs_names[[1]], c("file","sample", "n", "data","location","meta_name","dilution","analysis_date", "description","doc_mgL","notes"))


    eem_names <- lapply(add_metadata(metadata, example_eems), names)
    expect_length(eem_names[[1]], 14)
    expect_equal(eem_names[[1]], eem_names[[6]])
    expect_equal(eem_names[[1]], c("file","sample", "x", "ex", "em", "location","meta_name","dilution",
                                   "integration_time_s", "raman_area_1s", "analysis_date", "description","doc_mgL","notes"))

    abs_aug <- add_metadata(metadata, example_abs)
    eem_aug <- add_metadata(metadata, example_eems)

    expect_equal(get_sample_info(eem_aug, "meta_name"), rep(c("ExampleBlank", "ExampleTeaStd", "ExampleSample", "QSSRM250319"), each=2))

    expect_false(any(is.na(get_sample_info(eem_aug, "raman_area_1s")))) #make sure these aren't NA
    expect_false(any(is.na(get_sample_info(eem_aug, "integration_time_s")))) #make sure these aren't NA

  })

  # TODO update the ext data examples to test this
  test_that("Metadata adds sample type with and without sample_type in metadata", {
    meta_with_sample_type <- metadata
    meta_without_sample_type <- subset(metadata, select = -sample_type)

    # Example raw data has sample_type "none" initialized
    expect_equal(sapply(example_eems, attr, "sample_type"), rep('none', 8))
    expect_equal(sapply(example_abs,  attr, "sample_type"), rep('none', 4))

    # EEMs
    eems_w_meta_explicit <- add_metadata(meta_with_sample_type, example_eems)
    expect_warning(eems_w_meta_guessed <- add_metadata(meta_without_sample_type, example_eems),
                   "No sample_type in Metadata. Guessing sample_types by pattern matching data_identifier")
    expect_equal(sapply(eems_w_meta_explicit, attr, "sample_type"), c("iblank", "sblank", "iblank", "check", "iblank", "sample", "iblank", "sample"))
    expect_equal(sapply(eems_w_meta_guessed,  attr, "sample_type"), c("iblank", "sblank", "iblank", "check", "iblank", "sample", "iblank", "sample"))

    # ABS
    abs_w_meta_explicit <- add_metadata(meta_with_sample_type, example_abs)
    expect_warning(abs_w_meta_guessed <- add_metadata(meta_without_sample_type, example_abs),
                   "No sample_type in Metadata. Guessing sample_types by pattern matching data_identifier")
    expect_equal(sapply(abs_w_meta_explicit, attr, "sample_type"), c("sblank", "check", "sample", "sample"))
    expect_equal(sapply(abs_w_meta_guessed,  attr, "sample_type"), c("sblank", "check", "sample", "sample"))

  })

  test_that("metadata add works with names contining each other", {
   #make contained names
    meta <- eemanalyzeR::metadata
    meta$data_identifier[2] <- "ExampleSample1"

    example_eems[[3]]$sample <- gsub("TeaStd", "Sample1", example_eems[[3]]$sample)
    example_eems[[4]]$sample <- gsub("TeaStd", "Sample1", example_eems[[4]]$sample)

    expect_no_error(add_metadata(meta, example_eems))
 })
