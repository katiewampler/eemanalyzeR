test_that("Instrument blank is accepted and valid blanklist returned", {
  eems <- add_metadata(metadata, example_eems)
  test_blanklist <- subset_type(eems, type = c("iblank", "sblank"))
  
  # Do some mocked bindings here
  local_mocked_bindings(.yesorno = function(...) TRUE)
  valid_blanklist <- validate_blanks(test_blanklist)

  # Check the object's class and names
  expect_true(class(valid_blanklist) == "eemlist")
  expect_true(all(get_sample_info(valid_blanklist, "sample") == c("B1S1ExampleBlankBEM", "ManualExampleTeaWaterfallPlotBlank")))


})

test_that("Instrument blank not accepted, but sample blank accepted and valid blanklist returned", {
  eems <- add_metadata(metadata, example_eems)
  test_blanklist <- subset_type(eems, type = c("iblank", "sblank"))
  
  # Do some mocked bindings here
  local_mocked_bindings(.yesorno = mock_output_sequence(FALSE, TRUE))
  valid_blanklist <- validate_blanks(test_blanklist)

  expect_true(class(valid_blanklist) == "eemlist")
  expect_true(all(get_sample_info(valid_blanklist, "sample") == "B1S1ExampleBlankSEM"))

})

test_that("Processing aborts if no blanks are accepted", {
  eems <- add_metadata(metadata, example_eems)
  test_blanklist <- subset_type(eems, type = c("iblank", "sblank"))
  
  # Do some mocked bindings here
  local_mocked_bindings(.yesorno = mock_output_sequence(FALSE, FALSE))
  expect_error(validate_blanks(test_blanklist), "No valid instrument or sample blanks found. Aborting processing")

})
