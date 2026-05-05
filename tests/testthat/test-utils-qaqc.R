#test that method selection works
test_that("correct mdls are selected", {
  #no dir -> skip QAQC checks
  missing_dir <- get_qaqc(NA, "mdl")

  expect_equal(missing_dir$eem_mdl, NULL)
  expect_equal(missing_dir$abs_mdl, NULL)

  set_readme(NULL)

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

  reset_session_config()
  #multiple sets in dir -> return after asking in interactive, otherwise use default with warning
  dir.create(file.path(dummy_dir,"method1"), showWarnings = FALSE, recursive = TRUE)
  file.copy(stds, file.path(dummy_dir,"method1", gsub("default", "method1", basename(stds))))

  expect_warning(multiple_methods <- get_qaqc(dummy_dir, type="mdl"), "Running non-interactively; default QAQC method files were used.")

  #check readme (should have written methods)
  check <- get_readme()
  expect_true(grepl("using method default", check$mdl))
})
