# Tests for utils needs to be built up as we add functions and merge branches
# Overloaded subsetters ------------------------------
# Test eemslist and abslist `[` overloading
test_that("successfull eemlist subsetting", {
          expect_true(class(example_eems[1]) == "eemlist")
          expect_true(class(example_abs[1]) == "abslist")
          })

# Checkers -------------------------------------------
# Test .is_eem checker
test_that(".is_eem works",
  {
    expect_true(.is_eem(example_eems[[1]]))
    expect_false(.is_eem(example_eems))
    expect_false(.is_eem(example_abs))
  })

# Test .is_eemlist checker
test_that(".is_eemlist works",
  {
    expect_true(.is_eemlist(example_eems))
    expect_false(.is_eemlist(example_eems[[1]])) # just an eem
  })

# Test .is_abs checker
test_that(".is_abs works",
  {
    expect_true(.is_abs(example_abs[[1]]))
    expect_false(.is_abs(example_abs))
  })

# Test .is_abslist checker
test_that(".is_abslist works",
  {
    expect_true(.is_abslist(example_abs))
    expect_false(.is_eemlist(example_abs[[1]])) # just an absorbance
  })

# Documentaion utils --------------------------------
test_that("eemanalzyer versioning function works",
  {
    expect_match(.eemanalyzeR_ver(), "eemanalyzeR [0-9]\\.[0-9]\\.[0-9]")
  })

