# Using rlang::is_interactive() allows user to decide interactive or batch processing
# at the start of the processing run.

#test that gives error if blanklist is NULL isn't added
    test_that("error is returned if no blanklist is provided", {
     expect_error(add_blanks(example_eems, blanklist = NULL), 
      "Invalid blanklist provided: blanklist is not an eem or eemlist.") # fails if provided blanklist isn't eem or eemlist
    })

# Add blanks checks that metadata has been added
    test_that("add_blanks checks that metadata has been added", {
      expect_error(
        add_blanks(example_eems, example_eems[1]),
        "metadata must be added to link the samples and blanks, please run 'eemanalyzeR::add_metadata' first"
      )
    })

# Blanks can be automatically detected then added to eemslist as long as metadata is added
    test_that("Blanks can be automatically detected then added to eemslist as long as metadata is added",{
      #gives error if names don't match
      eemlist <- add_metadata(metadata, example_eems)
      blanklist <- subset_type(eemlist, "iblank")
      augment_eemlist <- add_blanks(eemlist, blanklist)

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
      blanklist <- blanklist[c(3,1,2,4)]
      augment_eemlist <- add_blanks(eemlist, blanklist)
      expect_equal(get_sample_info(augment_eemlist, "blk_file"),
                   c("data-raw/B1S1ExampleBlankBEM.dat","data-raw/B1S2ExampleTeaStdBEM.dat",
                     "data-raw/B1S3ExampleSampleBEM.dat", "data-raw/ManualExampleTeaWaterfallPlotBlank.dat"))

    })

#blanks are added when a single blank is supplied
    test_that("blanks are added when a single blank is supplied",{
      eemlist <- add_metadata(metadata, example_eems)[c(1:6)]
      blanklist <- subset_type(eemlist, "iblank")
      augment_eemlist <- add_blanks(eemlist, blanklist[1])
      expect_s3_class(augment_eemlist, "eemlist")
      expect_equal(names(augment_eemlist[[1]]), c("file", "sample", "x", "ex", "em",
                                                  "location", "meta_name", "dilution",
                                                  "integration_time_s", "raman_area_1s",
                                                  "analysis_date", "description", "doc_mgL",
                                                  "notes", "blk_file", "blk_x"))      
      expect_equal(get_sample_info(augment_eemlist, "blk_file"),
                   rep("data-raw/B1S1ExampleBlankBEM.dat",5))
      expect_equal(as.vector(augment_eemlist[[1]]$blk_x),as.vector(example_eems[[1]]$x))

    })


#mismatched wavelengths gives an error
    test_that("mismatched wavelengths gives an error",{
      # 1st EEM doesn't match blank wavelengths
      eemlist <- add_metadata(metadata, example_eems)
      blanklist <- subset_type(eemlist, "iblank")
      eemlist[[2]]$ex <- eemlist[[2]]$ex + 2
      expect_error(add_blanks(eemlist, blanklist), "excitation and/or emission wavelengths as mismatched between sample and blank")

      # 1st Blank doesn't match EEMs wavelength
      eemlist <- add_metadata(metadata, example_eems)
      blanklist <- subset_type(eemlist, "iblank")
      blanklist[[1]]$ex <- blanklist[[1]]$ex + 2
      expect_error(add_blanks(eemlist, blanklist), "excitation and/or emission wavelengths as mismatched between sample and blank")

    })


#names of blanks don't match samples returns an error
    test_that("mismatched names gives an error",{
      # EEM has wrong name
      eemlist <- add_metadata(metadata, example_eems)
      blanklist <- subset_type(eemlist, "iblank")
      eemlist[[2]]$meta_name <- "wrong name"
      expect_error(add_blanks(eemlist, blanklist), "more than one blank was provided, but blank names do not match samples")

      # Blank has wrong name
      eemlist <- add_metadata(metadata, example_eems)
      blanklist <- subset_type(eemlist, "iblank")
      blanklist[[1]]$meta_name <- "wrong name"
      expect_error(add_blanks(eemlist, blanklist), "more than one blank was provided, but blank names do not match samples")
    })
