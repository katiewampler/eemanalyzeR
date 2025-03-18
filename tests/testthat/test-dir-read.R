#testing loading of eems
  test_that("loading eems gives eemlist", {
    expect_s3_class(eem_dir_read(system.file("extdata", package = "eemanalyzeR")), "eemlist")
  })

  test_that("all eems are loaded", {
    expect_length(eem_dir_read(system.file("extdata", package = "eemanalyzeR")), 6)
  })

  test_that("absorbance data throws warning", {
    expect_warning(eem_dir_read(system.file("extdata", package = "eemanalyzeR"), skip=NULL), "Unable to import file")
  })

  test_that("eems selection loading works", {
    expect_length(eem_dir_read(system.file("extdata", package = "eemanalyzeR"), pattern = "SEM"), 3)
    expect_length(eem_dir_read(system.file("extdata", package = "eemanalyzeR"), skip = "SEM|ABS|Abs"), 3)
  })

#testing loading absorbance data
  test_that("all absorbance data loads", {
    expect_s3_class(abs_dir_read(system.file("extdata", package = "eemanalyzeR")),"abslist")
    expect_s3_class(abs_dir_read(system.file("extdata", package = "eemanalyzeR"))[[1]],"abs")
  })

  test_that("eems data throws warning", {
    expect_warning(abs_dir_read(system.file("extdata", package = "eemanalyzeR"), skip=NULL), "Unable to import file")
  })

  test_that("abs selection loading works", {
    expect_equal(length(abs_dir_read(system.file("extdata", package = "eemanalyzeR"), pattern = "Abs")), 1)
    expect_equal(length(abs_dir_read(system.file("extdata", package = "eemanalyzeR"), skip = "manual|BEM|SEM")), 4)
  })

