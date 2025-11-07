test_that("flattening and unflattening works", {
  flat_eem <- eem_transform(example_eems[[1]])
  eem_obj <- eem_transform(flat_eem)

  #checking the flat eem goes back to expected
    expect_false(identical(example_eems[[1]], eem_obj)) #won't perfectly match because we're missing the file info
    expect_true(identical(example_eems[[1]]$x, eem_obj$x))

    eem_obj <- eem_transform(flat_eem, file = example_eems[[1]]$file,
                             sample = example_eems[[1]]$sample, location=example_eems[[1]]$location)

    expect_true(identical(example_eems[[1]], eem_obj)) #should match perfectly if we add the additional info

  #check that we're getting a flat eem
    expect_equal(dim(flat_eem), c(1344, 3))
    expect_equal(colnames(flat_eem), c("ex", "em", "fluor"))

})
