#note: due to the user input, tests are quite challenging to write as it requires the relatively new
  #with_mocked_bindings function from withr. It took a while to figure out how to use the function, but finally
  #the solution from snaut here worked: https://stackoverflow.com/questions/51294489/how-to-test-behavior-that-depends-on-a-package-being-installed-or-not
with_mocked_bindings(
  .yesorno = function(question,
                      y_response,
                      n_response) TRUE,
  test_that("mocking works",{
    expect_equal(.yesorno("testing", "yes", "no"), TRUE)

  })
)


#test that gives error if metadata isn't added
with_mocked_bindings(
  .yesorno = function(question,
                      y_response,
                      n_response) TRUE,
    test_that("error is returned if samples don't have metadata added", {
     expect_error(add_blanks(example_eems), "metadata must be added to link the samples and blanks") #fails if more than one blank
     expect_no_error(add_blanks(example_eems, example_eems[[1]])) #runs if only one blank added
   })
)

#blanks are added when a list of eems is supplied
  with_mocked_bindings(
    .yesorno = function(question,
                        y_response,
                        n_response) TRUE,
    test_that("blanks are added when a list of blanks is supplied",{
      #gives error if names don't match
      eemlist <- add_metadata(metadata, example_eems)
      augment_eemlist <- add_blanks(eemlist)

      expect_s3_class(augment_eemlist, "eemlist")
      expect_equal(names(augment_eemlist[[1]]), c("file", "sample", "x", "ex", "em",
                                                  "location", "meta_name", "dilution",
                                                  "analysis_date", "description", "doc_mgL",
                                                  "notes", "blk_file", "blk_x"))
      expect_equal(get_sample_info(augment_eemlist, "blk_file"),
                   c("data-raw/B1S1ExampleBlankBEM.dat","data-raw/B1S2ExampleTeaStdBEM.dat",
                     "data-raw/B1S3ExampleSampleBEM.dat"))
      expect_equal(as.vector(augment_eemlist[[1]]$blk_x),as.vector(example_eems[[1]]$x))

    })
  )

#blanks are added when a list of blanks is supplied
  with_mocked_bindings(
    .yesorno = function(question,
                        y_response,
                        n_response) TRUE,
    test_that("blanks are added when a list of blanks is supplied",{
      #gives error if names don't match
      eemlist <- add_metadata(metadata, example_eems)
      samples <- eem_rm_blank(eemlist)
      blanks <- eem_get_blank(eemlist)
      augment_eemlist <- add_blanks(samples, blanks)

      expect_s3_class(augment_eemlist, "eemlist")
      expect_equal(names(augment_eemlist[[1]]), c("file", "sample", "x", "ex", "em",
                                                  "location", "meta_name", "dilution",
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
)

#blanks are added when a single blank is supplied
  with_mocked_bindings(
    .yesorno = function(question,
                        y_response,
                        n_response) TRUE,
    test_that("blanks are added when a single blank is supplied",{
      #gives error if names don't match
      samples <- eem_rm_blank(example_eems)
      blanks <- eem_get_blank(example_eems)
      augment_eemlist <- add_blanks(samples, blanks[[1]])

      expect_s3_class(augment_eemlist, "eemlist")
      expect_equal(names(augment_eemlist[[1]]), c("file", "sample", "x", "ex", "em",
                                                  "location", "blk_file", "blk_x"))
      expect_equal(get_sample_info(augment_eemlist, "blk_file"),
                   rep("data-raw/B1S1ExampleBlankBEM.dat",3))
      expect_equal(as.vector(augment_eemlist[[1]]$blk_x),as.vector(example_eems[[1]]$x))

    })
  )


##a TRUE returns an eemlist
  with_mocked_bindings(
    .yesorno = function(question,
                        y_response,
                        n_response) TRUE,
    test_that("a TRUE returns an eemlist",{
      eemlist <- add_metadata(metadata, example_eems)
      eemlist <- add_blanks(eemlist)
      expect_s3_class(eemlist, "eemlist")
    })
  )

