test_that("we can get the mdl for eems data", {
  #set up testing data
  mdl <- readRDS(file.path(system.file("extdata", package = "eemanalyzeR"),"eem-mdl.rds"))
  abslist <- add_metadata(metadata, example_abs)
  eemlist <- add_metadata(metadata, example_eems)
  blanklist <- subset_type(eemlist, type = "iblank")
  eemlist <- add_blanks(eemlist, blanklist)

  #should return an error because it hasn't been processed the same as the mdl
  expect_error(check_eem_mdl(eemlist[[1]], mdl, ex = 270:280, em=300:320), "attr")

  #now process data
  expect_warning(eemlist <- process_eem(eemlist, abslist))

  #works with a single sample
  expect_equal(check_eem_mdl(eemlist[[1]], mdl, ex = 270:280, em=300:320), "MDL01")

  #or an eemlist
  expect_equal(check_eem_mdl(eemlist, mdl, ex = 270:280, em=300:320), c("MDL01", NA, NA, NA))

  #returns a table
  single_table <- check_eem_mdl(eemlist[[1]], mdl, ex = 270:280, em=300:320, vals=TRUE)
  expect_s3_class(single_table, "data.frame")
  expect_equal(colnames(single_table), c("ex", "em", "fluor", "mdl"))

  table <- check_eem_mdl(eemlist, mdl, ex = 270:280, em=300:320, vals=TRUE)
  expect_s3_class(table, "data.frame")
  expect_equal(colnames(table), c("ex", "em", "fluor", "mdl", "sample"))

  #ensure it return NA if no mdl provided
  expect_equal(check_eem_mdl(eemlist, ex = 270:280, em=300:320), c(NA, NA, NA, NA))

  #check that a partial MDL is flagged
    eemlist[[2]]$x[1,4] <- 0.000001 #set a low value so it spans the MDL in the region
    expect_equal(check_eem_mdl(eemlist, mdl, ex = 314, em=250:262)[2], "MDL02")

})


test_that("we can get the mdl for abs data", {
  #set up testing data
  mdl <- readRDS(file.path(system.file("extdata", package = "eemanalyzeR"),"abs-mdl.rds"))
  abslist <- example_processed_abs

  #works with a single sample
  expect_equal(check_abs_mdl(abslist[[1]], mdl, wl=254:260), "MDL01")

  #or an eemlist
  expect_equal(check_abs_mdl(abslist, mdl, wl=254:260), c("MDL01", NA, NA, NA))

  #returns a table
  single_table <- check_abs_mdl(abslist[[1]], mdl, wl=254:260, vals=TRUE)
  expect_s3_class(single_table, "data.frame")
  expect_equal(colnames(single_table), c("wl", "abs", "mdl"))

  table <- check_abs_mdl(abslist, mdl, wl=254:260, vals=TRUE)
  expect_s3_class(table, "data.frame")
  expect_equal(colnames(table), c("wl", "abs", "mdl", "sample"))

  #ensure it return NA if no mdl provided
  expect_equal(check_abs_mdl(abslist, wl=254:260), c(NA, NA, NA, NA))

  #check that a partial MDL is flagged
  abslist[[2]]$data[32,2] <- 0.000001 #set a low value so it spans the MDL in the region
  expect_equal(check_abs_mdl(abslist, mdl, wl=242:248)[2], "MDL02")

})

