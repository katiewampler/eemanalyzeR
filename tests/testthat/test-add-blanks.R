# Using rlang::is_interactive() allows user to decide interactive or batch processing
# at the start of the processing run.

#test that gives error if metadata isn't added
    test_that("error is returned if samples don't have metadata added", {
     expect_error(add_blanks(example_eems), "metadata must be added to link the samples and blanks") #fails if more than one blank
     expect_no_error(add_blanks(example_eems, example_eems[[1]])) #runs if only one blank added
   })


#blanks are added when a list of eems is supplied
    test_that("blanks are added when a list of blanks is supplied",{
      #gives error if names don't match
      eemlist <- add_metadata(metadata, example_eems)
      augment_eemlist <- add_blanks(eemlist)

      expect_s3_class(augment_eemlist, "eemlist")
      expect_equal(names(augment_eemlist[[1]]), c("file", "sample", "x", "ex", "em",
                                                  "location", "meta_name", "dilution",
                                                  "integration_time_s", "raman_area_1s",
                                                  "analysis_date", "description", "doc_mgL",
                                                  "notes", "blk_file", "blk_x"))
      expect_equal(get_sample_info(augment_eemlist, "blk_file"),
                   c("data-raw/B1S1ExampleBlankBEM.dat","data-raw/B1S2ExampleTeaStdBEM.dat",
                     "data-raw/B1S3ExampleSampleBEM.dat"))
      expect_equal(as.vector(augment_eemlist[[1]]$blk_x),as.vector(example_eems[[1]]$x))

    })


#blanks are added when a list of blanks is supplied
    test_that("blanks are added when a list of blanks is supplied",{
      #gives error if names don't match
      eemlist <- add_metadata(metadata, example_eems)
      samples <- subset_qaqc(eemlist, negate=TRUE)
      blanks <- subset_qaqc(eemlist)
      augment_eemlist <- add_blanks(samples, blanks)

      expect_s3_class(augment_eemlist, "eemlist")
      expect_equal(names(augment_eemlist[[1]]), c("file", "sample", "x", "ex", "em",
                                                  "location", "meta_name", "dilution",
                                                  "integration_time_s", "raman_area_1s",
                                                  "analysis_date", "description", "doc_mgL",
                                                  "notes", "blk_file", "blk_x"))
      expect_equal(get_sample_info(augment_eemlist, "blk_file"),
                   c("data-raw/B1S1ExampleBlankBEM.dat","data-raw/B1S2ExampleTeaStdBEM.dat",
                     "data-raw/B1S3ExampleSampleBEM.dat"))
      expect_equal(as.vector(augment_eemlist[[1]]$blk_x),as.vector(example_eems[[1]]$x))

      #matching is correct regardless of order
      blanks <- blanks[order(c(3,1,2))]
      class(blanks) <- "eemlist"
      augment_eemlist <- add_blanks(samples, blanks)
      expect_equal(get_sample_info(augment_eemlist, "blk_file"),
                   c("data-raw/B1S1ExampleBlankBEM.dat","data-raw/B1S2ExampleTeaStdBEM.dat",
                     "data-raw/B1S3ExampleSampleBEM.dat"))
    })

#blanks are added when a single blank is supplied
    test_that("blanks are added when a single blank is supplied",{
      #gives error if names don't match
      samples <- subset_qaqc(example_eems, negate = TRUE)
      blanks <- subset_qaqc(example_eems)
      augment_eemlist <- add_blanks(samples, blanks[[1]])

      expect_s3_class(augment_eemlist, "eemlist")
      expect_equal(names(augment_eemlist[[1]]), c("file", "sample", "x", "ex", "em",
                                                  "location", "blk_file", "blk_x"))
      expect_equal(get_sample_info(augment_eemlist, "blk_file"),
                   rep("data-raw/B1S1ExampleBlankBEM.dat",3))
      expect_equal(as.vector(augment_eemlist[[1]]$blk_x),as.vector(example_eems[[1]]$x))

    })


##a TRUE returns an eemlist
    test_that("a TRUE returns an eemlist",{
      eemlist <- add_metadata(metadata, example_eems)
      eemlist <- add_blanks(eemlist)
      expect_s3_class(eemlist, "eemlist")
    })

#mismatched wavelengths gives an error
    test_that("mismatched wavelengths gives an error",{
      eemlist <- add_metadata(metadata, example_eems)
      eemlist[[1]]$ex <- eemlist[[1]]$ex[-1]
      eemlist[[1]]$x <- eemlist[[1]]$x[,-1]
      expect_error(add_blanks(eemlist), "excitation and/or emission wavelengths as mismatched between sample and blank")
    })


#names of blanks don't match samples returns an error
    test_that("mismatched names gives an error",{
      eemlist <- add_metadata(metadata, example_eems)
      eemlist[[1]]$meta_name <- "wrong name"
      expect_error(add_blanks(eemlist), "more than one blank was provided, but blank names do not match samples")
    })


