#test functions with metadata/blanks added
with_mocked_bindings(
  .yesorno = function(question,
                      y_response,
                      n_response) TRUE,
 test_that("successfully checks augmenting", {
   expect_false(all(.meta_added(example_eems)))
   expect_false(.meta_added(example_eems[[1]]))

   expect_false(all(.blk_added(example_eems)))
   expect_false(.blk_added(example_eems[[1]]))

   eemlist <- add_metadata(metadata, example_eems)
   expect_true(all(.meta_added(eemlist)))
   expect_true(.meta_added(eemlist[[1]]))

   eemlist <- add_blanks(eemlist)
   expect_true(all(.blk_added(eemlist)))
   expect_true(.blk_added(eemlist[[1]]))

 })
)


# Test eemslist and abslist `[` overloading
test_that("successfull eemlist subsetting", {
          expect_true(class(example_eems[1]) == "eemlist")
          expect_true(class(example_absorbance[1]) == "abslist")
          })
