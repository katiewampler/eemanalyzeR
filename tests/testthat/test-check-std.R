test_that("tea checks work", {
  #check that error is thrown if processing is different
    expect_warning(flags <- check_std(example_eems, example_abs, qaqc_dir = system.file("extdata", package = "eemanalyzeR")),
                   "No check standard samples found")
    expect_true(unique(flags$sample_id) == "notea")

  #check when tea is fully out
    abs <- example_processed_abs
    abs[[2]]$data[,2] <- rep(1,  abs[[1]]$n)
    check <- check_std(example_processed_eems, abs, qaqc_dir = system.file("extdata", package = "eemanalyzeR"))
    check <- check[order(check$flag),]

    expect_s3_class(check, "data.frame")
    expect_equal(check$flag, c(rep("STD01", 9), rep(NA, 20)))

  #check when it's partially out
    abs <- example_processed_abs
    abs[[2]]$data[30:32,2] <- rep(0.09,  3)
    check <- check_std(example_processed_eems, abs, qaqc_dir = system.file("extdata", package = "eemanalyzeR"))
    check <- check[order(check$flag),]
    expect_equal(check$flag, c(rep("STD01", 9), rep(NA, 21)))

  #check that vals are returned
    check <- check_std(example_processed_eems, abs, qaqc_dir = system.file("extdata", package = "eemanalyzeR"), vals=TRUE)
    # This fails
    expect_equal(ncol(check), 7)

  #check when there are two tea samples
    abs <- example_processed_abs
    abs[[4]] <- example_processed_abs[[2]]
    abs[[4]]$sample_id <- "example_tea2"
    class(abs) <- "abslist"

    eems <- example_processed_eems
    eems[[4]] <- example_processed_eems[[2]]
    eems[[4]]$sample_id <- "example_tea2"
    class(eems) <- "eemlist"

    check <- check_std(eems, abs, qaqc_dir = system.file("extdata", package = "eemanalyzeR"))

    expect_equal(dim(check), c(30, 4))
    expect_equal(unique(check$sample_id), c("ExampleTeaStd", "example_tea2"))

  #check the readme
    expect_true(grepl("0% (n=8) of the absorbance indices", get_readme()$check_std, fixed=TRUE))

})

#test that method selection works
test_that("check works when there are multiple methods", {
  #make sure we have a flag
  abs <- example_processed_abs
  abs[[2]]$data[,2] <- rep(1,  abs[[1]]$n)

  #no dir -> skip QAQC checks
   no_check <- check_std(example_processed_eems, abs, qaqc_dir = NA)
    expect_true(all(is.na(no_check$flag)))


  #only one set in dir -> return without anything
    dummy_dir <- withr::local_tempfile()
    dir.create(file.path(dummy_dir,"default"), showWarnings = FALSE, recursive = TRUE)
    stds <- list.files(system.file("extdata", package = "eemanalyzeR"), pattern= "check-std.rds|mdl.rds",
                       full.names = TRUE, recursive=TRUE)
    file.copy(stds, file.path(dummy_dir,"default"))

    one_set <- check_std(example_processed_eems, abs, qaqc_dir = dummy_dir)

    expect_equal(sum(one_set$flag == "STD01", na.rm = TRUE),9)

  #multiple sets in dir -> return after asking in interactive, otherwise use default with warning
    dir.create(file.path(dummy_dir,"method1"), showWarnings = FALSE, recursive = TRUE)
    file.copy(stds, file.path(dummy_dir,"method1", gsub("default", "method1", basename(stds))))


  ### !!!! currently check_std also run get_index which calls mdl check -> so we get extra warnings
      #do we want quiet or to pass method?
    expect_warning(expect_warning(expect_warning(multiple_methods <- check_std(example_processed_eems, abs, qaqc_dir = dummy_dir),
                   "Running non-interactively; default QAQC method files were used.")))

})

