test_that("tea checks work", {

  #check that error is thrown if processing is different
  expect_error(check_tea_std(example_eems, example_abs, std_dir = system.file("extdata", package = "eemanalyzeR")), "processing steps are different")

  #check when tea is fully out
    abs <- example_processed_abs
    abs[[2]]$data[,2] <- rep(1,  abs[[1]]$n)
    check <- check_tea_std(example_processed_eems, abs, std_dir = system.file("extdata", package = "eemanalyzeR"))

    expect_s3_class(check, "data.frame")
    expect_equal(check$tea_flag, c(rep(NA, 11), rep("STD01", 3)))

  #check when it's partially out
    abs <- example_processed_abs
    abs[[2]]$data[30:32,2] <- rep(0.09,  3)
    check <- check_tea_std(example_processed_eems, abs, std_dir = system.file("extdata", package = "eemanalyzeR"))
    expect_equal(check$tea_flag, c(rep(NA, 11), "STD01", "STD01", NA, "STD01"))

  #check that vals are returned
    check <- check_tea_std(example_processed_eems, abs, std_dir = system.file("extdata", package = "eemanalyzeR"), vals=TRUE)
    expect_equal(dim(check), c(15,7))

  #check when there are two tea samples
    abs <- example_processed_abs
    abs[[4]] <- example_processed_abs[[2]]
    abs[[4]]$meta_name <- "example_tea2"
    class(abs) <- "abslist"

    eems <- example_processed_eems
    eems[[4]] <- example_processed_eems[[2]]
    eems[[4]]$meta_name <- "example_tea2"
    class(eems) <- "eemlist"

    check <- check_tea_std(eems, abs, std_dir = system.file("extdata", package = "eemanalyzeR"))

    expect_equal(dim(check), c(30, 4))
    expect_equal(unique(check$meta_name), c("ExampleTeaStd", "example_tea2"))

  #check the readme
    expect_true(grepl("0% (n=8) of the absorbance indices", readme$check_std, fixed=TRUE))

  })

