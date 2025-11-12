# Test eemslist and abslist `[` overloading
test_that("successfull eemlist subsetting", {
          expect_true(class(example_eems[1]) == "eemlist")
          expect_true(class(example_absorbance[1]) == "abslist")
          })
