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

#test that method selection works
test_that("correct mdls are selected", {
  #no dir -> skip QAQC checks
    expect_warning(expect_warning(missing_dir <- get_qaqc(NA, "mdl"),
                                  "Fluorescence method detection limits"),
                                  "Absorbance method detection limits")

    expect_equal(missing_dir$eem_mdl, NULL)
    expect_equal(missing_dir$abs_mdl, NULL)

  #only one set in dir -> return without anything
    dummy_dir <- withr::local_tempfile()
    dir.create(file.path(dummy_dir,"default"), showWarnings = FALSE, recursive = TRUE)
    stds <- list.files(system.file("extdata", package = "eemanalyzeR"), pattern= "check-std.rds|mdl.rds", full.names = TRUE)
    file.copy(stds, file.path(dummy_dir,"default"))

    one_set <- get_qaqc(dummy_dir, type="check-std")

    expect_true(.is_eem(one_set$eem_check_std))
    expect_true(.is_abs(one_set$abs_check_std))

    expect_equal(one_set$eem_check_std, readRDS(stds[3]))
    expect_equal(one_set$abs_check_std, readRDS(stds[1]))

  #multiple sets in dir -> return after asking in interactive, otherwise use default with warning
    dir.create(file.path(dummy_dir,"method1"), showWarnings = FALSE, recursive = TRUE)
    file.copy(stds, file.path(dummy_dir,"method1", gsub("default", "method1", basename(stds))))

    expect_warning(multiple_methods <- get_qaqc(dummy_dir, type="mdl"), "Running non-interactively; default QAQC method files were used.")

    #check readme (should have written methods)
    check <- get_readme()
    expect_true(grepl("using method default", check$mdl))
})
