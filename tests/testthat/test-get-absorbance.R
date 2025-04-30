test_that("errors are returned", {
  expect_error(get_absorbance(example_eems), ".is_abs")
  expect_error(get_absorbance(example_absorbance, wl="not_number"), "is.numeric")
  expect_error(get_absorbance(example_absorbance, wl=254, cuvle = "not_number"), "is.numeric")
  expect_error(get_absorbance(example_absorbance, wl=254, suva="not_logical"), "is.logical")
})

test_that("correct values are returned", {
  abslist <- add_metadata(metadata, example_absorbance)
  expect_equal(get_absorbance(abslist, 254), c("0.00054853647738303", "0.163215306070913","0.0806502356009794"))
  expect_equal(get_absorbance(abslist, 254, suva=TRUE), c( "DOC01","DOC01" , "4.15722863922574"))
})
