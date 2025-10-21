test_that("we can get the mdl for eems data", {
  #set up testing data
  mdl <- readRDS(file.path(system.file("extdata", package = "eemanalyzeR"),"eem-mdl.rds"))
  abslist <- add_metadata(metadata, example_absorbance)
  eemlist <- add_metadata(metadata, example_eems)
  eemlist <- add_blanks(eemlist, validate=FALSE)

  #should return an error because it hasn't been processed the same as the mdl
  expect_error(check_eem_mdl(eemlist[[1]], mdl, ex = 270:280, em=300:320), "attr")

  #now process data
  expect_warning(eemlist <- process_eem(eemlist, abslist))

  #works with a single sample
  expect_false(check_eem_mdl(eemlist[[1]], mdl, ex = 270:280, em=300:320))

  #or an eemlist
  expect_equal(check_eem_mdl(eemlist, mdl, ex = 270:280, em=300:320), c(FALSE, TRUE, TRUE))

  #returns a table
  single_table <- check_eem_mdl(eemlist[[1]], mdl, ex = 270:280, em=300:320, vals=TRUE)
  expect_s3_class(single_table, "data.frame")
  expect_equal(colnames(single_table), c("ex", "em", "fluor", "mdl"))

  table <- check_eem_mdl(eemlist, mdl, ex = 270:280, em=300:320, vals=TRUE)
  expect_s3_class(table, "data.frame")
  expect_equal(colnames(table), c("ex", "em", "fluor", "mdl", "sample"))

})
