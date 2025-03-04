test_that("blank subtract returns eem/eemlist", {
  eem <- add_metadata(metadata,example_eems)
  eem <- add_blanks(eem)
  expect_s3_class(blk_subtract(eem[[1]]), "eem")
  expect_s3_class(blk_subtract(eem), "eemlist")
})

test_that("warnings are given for blank subtraction", {
 eem <- add_metadata(metadata,example_eems)
 expect_warning(blk_subtract(eem), "Missing blank data from eem or eemlist")
 expect_error(blk_subtract(eem[[1]]), "Missing blank data from eem or eemlist")
})

test_that("blank subtraction occurs correctly", {
  eem <- add_metadata(metadata,example_eems)
  eem <- add_blanks(eem)
  eem_sub <- blk_subtract(eem[[1]])

  expect_true(.eem_equal(eem_sub$x, (eem[[1]]$x-eem[[1]]$blk_x)))

})

test_that("subtraction is tracked",{
  eem <- add_metadata(metadata,example_eems)
  eem <- add_blanks(eem)
  expect_true(attr(blk_subtract(eem[[1]]), "is_blank_corrected"))
  expect_true(all(sapply(blk_subtract(eem), attr, "is_blank_corrected")))
})

test_that("blank subtract only occurs once",{
  eem <- add_metadata(metadata,example_eems)
  eem <- add_blanks(eem)
  eem <- blk_subtract(eem)

  eem_sub2 <- blk_subtract(eem)
  expect_true(.eem_equal(blk_subtract(eem)[[1]]$x, eem[[1]]$x))

  eem_sub2 <- blk_subtract(eem[[1]])
  expect_true(.eem_equal(eem_sub2$x, eem[[1]]$x))
})
