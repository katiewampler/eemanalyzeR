#compare with existing index function when possible to confirm values
test_that("absorbance indices are correct", {
  #get indices
    abslist <- example_processed_abs
    eemlist <- example_processed_eems
    mdl_dir <- system.file("extdata", package = "eemanalyzeR")

    #should give warnings about no using mdl
    expect_warning(expect_warning(indices <- eemanalyzeR_indices(eemlist, abslist), "fluorescence"), "absorbance")

    indices <- eemanalyzeR_indices(eemlist, abslist, mdl_dir = mdl_dir)
    index <- indices$abs_index #get abs indices
    index$value <- gsub("_MDL02", "", index$value)
    index$value[grep("DOC|DATA|MDL", index$value)] <- NA
    index$value <- as.numeric(index$value) #make numeric, can't do in function because flags

  #get stardom values
    abs_interp <- lapply(abslist, function(x){
      abs <- data.frame(x$data)
      abs_filled <- merge(abs, data.frame(X1=min(abs$X1):max(abs$X1)), all=T)
      abs_filled <- zoo::na.approx(abs_filled)
      x$data <- as.matrix(abs_filled)
      x$n <- nrow(abs_filled)
      return(x)
    })
    class(abs_interp) <- "abslist"
    raw_abs <- get_sample_info(abs_interp, "data")
    stardom_index <- staRdom::abs_parms(raw_abs, cuvle=1, unit = "absorbance", add_as=c(280,412), cores=1)

  #remove NA values
    index <- index[!is.na(index$value),]

  #SUVA Values (stardom seems to add an extra log(10) that shouldn't be there and multiples by 100 already)
    doc_abs <- get_sample_info(abslist, "doc_mgL")
    expect_equal(index$value[index$index == "SUVA254"], (stardom_index$a254/doc_abs/log(10))[3], tolerance=1e-5)
    expect_equal(index$value[index$index == "SUVA280"], (stardom_index$a280/doc_abs/log(10))[3], tolerance=1e-5)
    expect_equal(index$value[index$index == "SVA412"], (stardom_index$a412/doc_abs/log(10))[3], tolerance=1e-5)

  #spectral slopes
    expect_equal(index$value[index$index == "S275_295"], stardom_index$S275_295[2:3], tolerance=1e-5)
    expect_equal(index$value[index$index == "S350_400"], stardom_index$S350_400, tolerance=1e-5)
    expect_equal(index$value[index$index == "SR"], stardom_index$SR[2:3], tolerance=1e-5)

  #ratios
    expect_equal(index$value[index$index == "E2_E3"], stardom_index$E2_E3[2:3], tolerance=1e-5)
    expect_equal(index$value[index$index == "E4_E6"], stardom_index$E4_E6[2:3], tolerance=1e-5)
})

test_that("eems indices are correct", {
  #get indices
  abslist <- example_processed_abs
  eemlist <- example_processed_eems
  mdl_dir <- system.file("extdata", package = "eemanalyzeR")

  indices <- eemanalyzeR_indices(eemlist, abslist, mdl_dir = mdl_dir)

  #compare values to see if anything changed
  expect_snapshot(indices$eem_index)

})

test_that("output is correct",{
  abslist <- example_processed_abs
  eemlist <- example_processed_eems
  mdl_dir <- system.file("extdata", package = "eemanalyzeR")
  indices <- eemanalyzeR_indices(eemlist, abslist, mdl_dir = mdl_dir)

  expect_equal(class(indices), "list")
  expect_length(indices, 2)
  expect_equal(class(indices[[1]]), "data.frame")

})
