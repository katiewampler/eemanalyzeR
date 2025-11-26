test_that("validate blank works as expected", {
  eems <- add_metadata(metadata, example_eems)
  eems <- subset_type(eems, type = "iblank")
  continue <- validate_blanks(eems)
  expect_true(continue)
})
