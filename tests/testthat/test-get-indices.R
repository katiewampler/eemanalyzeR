#TODO: check that different methods work

  test_that("missing wavelengths don't break indices code", {
    abslist <- add_metadata(metadata, example_absorbance)
    eemlist <- add_metadata(metadata, example_eems)
    eemlist <- eemR::eem_cut(eemlist, ex=400:900, em=400:900, exact=F) #cut down so there's missing wavelengths

    expect_warning(indices <- get_indices(eemlist, abslist), "Data has not been processed")

          })

  test_that("removing doc normalized data works", {
    abslist <- add_metadata(metadata, example_absorbance)
    eemlist <- add_metadata(metadata, example_eems)
    doc <- ifelse(is.na(get_sample_info(eemlist, "doc_mgL")), 1, get_sample_info(eemlist, "doc_mgL"))
    eemlist_doc <- eem_normalize(eemlist, doc)
    eemlist_doc[5:6] <- lapply(eemlist_doc[5:6], function(x){
                               attr(x,"is_doc_normalized") <- TRUE
                               return(x)})

    expect_false(.eem_equal(eemlist[[6]]$x, eemlist_doc[[6]]$x)) #ensure normalization is done

    expect_warning(indices <- get_indices(eemlist, abslist), "Data has not been processed")
    expect_warning(indices2 <- get_indices(eemlist_doc, abslist), "Data has not been processed")

    expect_equal(indices$eem_index, indices2$eem_index)
  })

  test_that("format is correct", {
    abslist <- add_metadata(metadata, example_absorbance)
    eemlist <- add_metadata(metadata, example_eems)

    expect_warning(indices <- get_indices(eemlist, abslist), "Data has not been processed")
    expect_length(indices, 2)
    expect_s3_class(indices$abs_index, "data.frame")
    expect_s3_class(indices$eem_index, "data.frame")

    #for long
    expect_equal(ncol(indices$abs_index), 5)
    expect_equal(ncol(indices$eem_index), 5)
    expect_equal(class(indices$abs_index$value), "numeric")
    expect_equal(class(indices$eem_index$value), "numeric")
    expect_equal(class(indices$abs_index$QAQC_flag), "character")
    expect_equal(class(indices$abs_index$QAQC_flag), "character")
    expect_false(any(is.na(indices$abs_index$value)))
    expect_false(any(is.na(indices$eem_index$value)))
    expect_false(any(is.na(indices$abs_index$QAQC_flag)))
    expect_false(any(is.na(indices$eem_index$QAQC_flag)))


    #for wide
    expect_warning(indices <- get_indices(eemlist, abslist, return="wide"), "Data has not been processed")
    expect_equal(nrow(indices$abs_index), 3)
    expect_equal(nrow(indices$eem_index), 6)
    expect_true(all(apply(indices$abs_index, 1, class) == "character"))
    expect_true(all(apply(indices$eem_index, 1, class) == "character"))

    expect_length(unique(indices$eem_index$meta_name), 3)
    expect_length(unique(indices$abs_index$meta_name), 3)

    expect_length(unique(indices$abs_index$sample_name), 3)
    expect_length(unique(indices$eem_index$sample_name), 6)


  })

  test_that("NA index doesn't break anything",{
    return_NA <- function(eemlist, abslist, cuvle=1){
      return(list(abs_index=NA, eem_index=NA))}

    expect_no_error(expect_warning(get_indices(example_eems, example_absorbance, index_method=return_NA)))
  })

  test_that("warning is returned if some steps are done",{
    eemlist <- add_metadata(metadata, example_eems)
    eemlist <- raman_normalize(eemlist)

    expect_warning(get_indices(eemlist, example_absorbance), "Data has not been fully processed")
  })

  test_that("flags are correct", {
    abslist <- add_metadata(metadata, example_absorbance)
    eemlist <- add_metadata(metadata, example_eems)
    eemlist <- eemR::eem_cut(eemlist, ex=500:900, em=500:900, exact=F) #cut down so there's missing wavelengths

    expect_warning(indices <- get_indices(eemlist, abslist), "Data has not been processed")

    #indices$eem_index$QAQC_flag
  })
