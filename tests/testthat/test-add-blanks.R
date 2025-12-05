# Using rlang::is_interactive() allows user to decide interactive or batch processing
# at the start of the processing run.

#test that gives error if metadata isn't added
    test_that("error is returned if samples don't have metadata added", {
     expect_error(add_blanks(example_eems, validate = FALSE), "eemlist or blanklist had zero samples") #fails if more than one blank
     expect_no_error(add_blanks(example_eems, example_eems[[1]], validate=FALSE)) #runs if only one blank added
   })


#blanks are added when a list of eems is supplied
    test_that("blanks are added when a list of blanks is supplied",{
      #gives error if names don't match
      eemlist <- add_metadata(metadata, example_eems)
      augment_eemlist <- add_blanks(eemlist, validate = FALSE)

      expect_s3_class(augment_eemlist, "eemlist")
      expect_equal(names(augment_eemlist[[1]]), c("file", "sample", "x", "ex", "em",
                                                  "location", "meta_name", "dilution",
                                                  "integration_time_s", "raman_area_1s",
                                                  "analysis_date", "description", "doc_mgL",
                                                  "notes", "blk_file", "blk_x"))
      expect_equal(get_sample_info(augment_eemlist, "blk_file"),
                   c("data-raw/B1S1ExampleBlankBEM.dat","data-raw/B1S2ExampleTeaStdBEM.dat",
                     "data-raw/B1S3ExampleSampleBEM.dat", "data-raw/ManualExampleTeaWaterfallPlotBlank.dat"))
      expect_equal(as.vector(augment_eemlist[[1]]$blk_x),as.vector(example_eems[[1]]$x))

    })


#blanks are added when a list of blanks is supplied
    test_that("blanks are added when a list of blanks is supplied",{
      #gives error if names don't match
      eemlist <- add_metadata(metadata, example_eems)
      samples <- subset_type(eemlist, type="iblank", negate = TRUE)
      blanks <- subset_type(eemlist, type="iblank")
      augment_eemlist <- add_blanks(samples, blanks, validate = FALSE)

      expect_s3_class(augment_eemlist, "eemlist")
      expect_equal(names(augment_eemlist[[1]]), c("file", "sample", "x", "ex", "em",
                                                  "location", "meta_name", "dilution",
                                                  "integration_time_s", "raman_area_1s",
                                                  "analysis_date", "description", "doc_mgL",
                                                  "notes", "blk_file", "blk_x"))
      expect_equal(get_sample_info(augment_eemlist, "blk_file"),
                   c("data-raw/B1S1ExampleBlankBEM.dat","data-raw/B1S2ExampleTeaStdBEM.dat",
                     "data-raw/B1S3ExampleSampleBEM.dat", "data-raw/ManualExampleTeaWaterfallPlotBlank.dat"))
      expect_equal(as.vector(augment_eemlist[[1]]$blk_x),as.vector(example_eems[[1]]$x))

      #matching is correct regardless of order
      blanks <- blanks[order(c(3,1,2,4))]
      class(blanks) <- "eemlist"
      augment_eemlist <- add_blanks(samples, blanks, validate = FALSE)
      expect_equal(get_sample_info(augment_eemlist, "blk_file"),
                   c("data-raw/B1S1ExampleBlankBEM.dat","data-raw/B1S2ExampleTeaStdBEM.dat",
                     "data-raw/B1S3ExampleSampleBEM.dat", "data-raw/ManualExampleTeaWaterfallPlotBlank.dat"))
    })

#blanks are added when a single blank is supplied
    test_that("blanks are added when a single blank is supplied",{
      #gives error if names don't match
      eemlist <- add_metadata(metadata, example_eems)

      samples <- .make_base_eem(subset_type(eemlist, type="iblank", negate = TRUE))
      blanks <- .make_base_eem(subset_type(eemlist, type="iblank"))
      augment_eemlist <- add_blanks(samples, blanks[[1]], validate=FALSE)

      expect_s3_class(augment_eemlist, "eemlist")
      expect_equal(names(augment_eemlist[[1]]), c("file", "sample", "x", "ex", "em",
                                                  "location", "blk_file", "blk_x"))
      expect_equal(get_sample_info(augment_eemlist, "blk_file"),
                   rep("data-raw/B1S1ExampleBlankBEM.dat",4))
      expect_equal(as.vector(augment_eemlist[[1]]$blk_x),as.vector(example_eems[[1]]$x))

    })


##a TRUE returns an eemlist
    test_that("a TRUE returns an eemlist",{
      eemlist <- add_metadata(metadata, example_eems)
      eemlist <- add_blanks(eemlist, validate = FALSE)
      expect_s3_class(eemlist, "eemlist")
    })

#mismatched wavelengths gives an error
    test_that("mismatched wavelengths gives an error",{
      eemlist <- add_metadata(metadata, example_eems)
      eemlist[[1]]$ex <- eemlist[[1]]$ex[-1]
      eemlist[[1]]$x <- eemlist[[1]]$x[,-1]
      expect_error(add_blanks(eemlist, validate = FALSE), "excitation and/or emission wavelengths as mismatched between sample and blank")
    })


#names of blanks don't match samples returns an error
    test_that("mismatched names gives an error",{
      eemlist <- add_metadata(metadata, example_eems)
      eemlist[[1]]$meta_name <- "wrong name"
      expect_error(add_blanks(eemlist, validate = FALSE), "more than one blank was provided, but blank names do not match samples")
    })

#using a sample blank works
    test_that("replacing the blank works",{
      eemlist <- add_metadata(metadata, example_eems)

      #create two blanks for checking
        eemlist[9:10] <- eemlist[1:2]
        eemlist[[9]]$meta_name <- eemlist[[10]]$meta_name <- "ExampleBlank2"

      #not accept all
        with_mocked_bindings(
          validate_blanks = function(eemlist) FALSE,
          {
            expect_error(add_blanks(eemlist), "Processing stopped by user")
          })

      #pretend to skip the first and accept the second
        with_mocked_bindings(
          validate_blanks = function(eemlist) if(all(sapply(eemlist, attr, "sample_type") == "iblank")){FALSE}else{TRUE},
          {
            #fails because integration times aren't the same
            expect_error(add_blanks(eemlist), "integration times are not the same between the sample and the blank")

            eemlist[7:8] <- NULL
            list_with_blanks <- add_blanks(eemlist)
          })

       expect_equal(unique(get_sample_info(list_with_blanks, "blk_file")), "data-raw/B1S1ExampleBlankSEM.dat")
       expect_true(grepl("Instrument blank was replaced with analytical blank: ExampleBlank", get_readme()$eem_add_blank))
    })

