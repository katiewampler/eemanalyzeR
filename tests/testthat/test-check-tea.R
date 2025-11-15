test_that("multiplication works", {

  #get tea standards
  eem_std <- readRDS(file.path(system.file("extdata", package = "eemanalyzeR"),
  "eem-tea-std.rds"))
  abs_std <- readRDS(file.path(system.file("extdata", package = "eemanalyzeR"),
  "abs-tea-std.rds"))

  #check that error is thrown if processing is different
  expect_error(check_tea_std(example_eems, example_abs, std_dir = system.file("extdata", package = "eemanalyzeR")), "processing steps are different")

  #check when tea is out
    abs <- example_processed_abs
    abs[[2]]$data[,2] <- rep(1,  abs[[1]]$n)
    check <- check_tea_std(example_processed_eems, abs, std_dir = system.file("extdata", package = "eemanalyzeR"))
    expect_true(check$tea_flag[1])
    expect_equal(check$per_out, c(1,0))


  #check that vals are returned
    check <- check_tea_std(example_processed_eems, abs, std_dir = system.file("extdata", package = "eemanalyzeR"), vals=TRUE)
    expect_equal(dim(check), c(14,7))

  })

