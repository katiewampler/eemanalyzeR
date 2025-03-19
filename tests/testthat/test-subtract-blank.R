test_that("blank subtract returns eem/eemlist", {
  eem <- add_metadata(metadata,example_eems)
  eem <- add_blanks(eem)
  expect_s3_class(subtract_blank(eem[[1]]), "eem")
  expect_s3_class(subtract_blank(eem), "eemlist")
})

test_that("warnings are given for blank subtraction", {
 eem <- add_metadata(metadata,example_eems)
 expect_warning(subtract_blank(eem), "Missing blank data from eem or eemlist")
 expect_error(subtract_blank(eem[[1]]), "Missing blank data from eem or eemlist")
})

test_that("blank subtraction occurs correctly", {
  eem <- add_metadata(metadata,example_eems)
  eem <- add_blanks(eem)
  eem_sub <- subtract_blank(eem[[1]])

  expect_true(.eem_equal(eem_sub$x, (eem[[1]]$x-eem[[1]]$blk_x)))

})

test_that("subtraction is tracked",{
  eem <- add_metadata(metadata,example_eems)
  eem <- add_blanks(eem)
  expect_true(attr(subtract_blank(eem[[1]]), "is_blank_corrected"))
  expect_true(all(sapply(subtract_blank(eem), attr, "is_blank_corrected")))
})

test_that("blank subtract only occurs once",{
  eem <- add_metadata(metadata,example_eems)
  eem <- add_blanks(eem)
  eem <- subtract_blank(eem)

  eem_sub2 <- subtract_blank(eem)
  expect_true(.eem_equal(subtract_blank(eem)[[1]]$x, eem[[1]]$x))

  eem_sub2 <- subtract_blank(eem[[1]])
  expect_true(.eem_equal(eem_sub2$x, eem[[1]]$x))
})
