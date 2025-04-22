#TODO: test that removing DOC normalizing works as expected
#check that format is is as expected
#check that warning if not processed
#check that different methods work

  test_that("missing wavelengths don't break code", {
    abslist <- add_metadata(metadata, example_absorbance)
    eemlist <- add_metadata(metadata, example_eems)
    eemlist <- eemR::eem_cut(eemlist, ex=400:900, em=400:900, exact=F) #cut down so there's missing wavelengths

    expect_warning(indices <- get_indices(eemlist, abslist), "Data has not been processed")
    expect_length(indices, 2)

          })
