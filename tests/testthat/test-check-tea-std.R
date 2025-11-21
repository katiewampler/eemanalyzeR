test_that("tea checks work", {

  #check that error is thrown if processing is different
    expect_warning(flags <- check_tea_std(example_eems, example_abs, std_dir = system.file("extdata", package = "eemanalyzeR")), "No tea check standard samples found")
    expect_true(unique(flags$meta_name) == "notea")

  #check when tea is fully out
    abs <- example_processed_abs
    abs[[2]]$data[,2] <- rep(1,  abs[[1]]$n)
    check <- check_tea_std(example_processed_eems, abs, std_dir = system.file("extdata", package = "eemanalyzeR"))
    check <- check[order(check$tea_flag),]

    expect_s3_class(check, "data.frame")
    expect_equal(check$tea_flag, c(rep("STD01", 9), rep(NA, 20)))

  #check when it's partially out
    abs <- example_processed_abs
    abs[[2]]$data[30:32,2] <- rep(0.09,  3)
    check <- check_tea_std(example_processed_eems, abs, std_dir = system.file("extdata", package = "eemanalyzeR"))
    check <- check[order(check$tea_flag),]
    expect_equal(check$tea_flag, c(rep("STD01", 9), rep(NA, 21)))

  #check that vals are returned
    check <- check_tea_std(example_processed_eems, abs, std_dir = system.file("extdata", package = "eemanalyzeR"), vals=TRUE)
    # This fails
    expect_equal(dim(check), c(30,7))

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

