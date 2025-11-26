test_that("subsetting samples by info works", {
  # Subset by sample name
    names <- get_sample_info(example_eems, "sample")
    eem_subset <- subset_samples(example_eems, "sample", names[1], verbose = FALSE) # removes by default
    expect_equal(get_sample_info(example_eems, "sample")[-1], get_sample_info(eem_subset, "sample"))

    eem_subset <- subset_samples(example_eems, "sample", names[1], keep = TRUE, verbose = FALSE) # keeps instead
    expect_equal(get_sample_info(example_eems, "sample")[1], get_sample_info(eem_subset, "sample"))

  # Subset by metadata name
    eemlist <- add_metadata(metadata, example_eems)
    names <- get_sample_info(eemlist, "meta_name")
    eem_subset <- subset_samples(eemlist, "meta_name", names[1], verbose = FALSE) # removes by default
    expect_equal(get_sample_info(example_eems, "sample")[-c(1:2)], get_sample_info(eem_subset, "sample"))

  #if it doesn't match anything
    eem_subset <- subset_samples(example_eems, "sample", "noname", verbose = FALSE) # removes by default
    expect_length(eem_subset, 8)

  #if you ask for info that doesn't exist
    expect_error(eem_subset <- subset_samples(example_eems, "meta_name", "noname", verbose = FALSE))

  #check verbose works
    expect_message(eem_subset <- subset_samples(eemlist, "meta_name", names[1]))
    expect_no_message(eem_subset <- subset_samples(eemlist, "meta_name", names[1], verbose = FALSE))

  #case sensitivity works
    eemlist <- add_metadata(metadata, example_eems)
    names <- get_sample_info(eemlist, "meta_name")
    eem_subset <- subset_samples(eemlist, "meta_name", "BLANK", verbose = FALSE)
    expect_length(eem_subset, 8)

    eem_subset <- subset_samples(eemlist, "meta_name", "BLANK", ignore_case = TRUE, verbose = FALSE) # removes by default
    expect_length(eem_subset, 6)

})
