test_that("blank subtraction works", {
  eem <- add_metadata(metadata, example_eems)
  eem <- add_blanks(eem, validate = FALSE)

  # Subtract blank from a single EEM
    eem_sub <- subtract_blank(eem[[1]])
    expect_false(.eem_equal(eem_sub$x, eem[[1]]$x))

  # Subtract blank from an EEM list
    eemlist_sub <- subtract_blank(eem)
    expect_false(.eem_equal(eemlist_sub[[1]]$x, eem[[1]]$x))

  #warning if no blanks added
    eem <- add_metadata(metadata, example_eems)
    #can't run if only one sample added
    expect_error(eem_sub <- subtract_blank(eem[[1]]), "Missing blank data from eem or eemlist")

    #will try to add if not added blanks
    expect_warning(eem_sub <- subtract_blank(eem), "Missing blank data from eem or eemlist")


  #error if you can't add blanks
    eem <- example_eems
    expect_error(eem_sub <- subtract_blank(eem[[1]]), "Missing blank data from eem or eemlist")

})
