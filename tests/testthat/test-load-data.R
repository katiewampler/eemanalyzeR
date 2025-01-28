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

    test_that("eems pattern selection loading works", {
      expect_length(eem_dir_read(system.file("extdata", package = "eemanalyzeR"), pattern = "SEM"), 3)
    })

    test_that("eems skip exclusion loading works", {
      expect_length(eem_dir_read(system.file("extdata", package = "eemanalyzeR"), skip = "SEM|ABS"), 3)
    })

#testing loading absorbance data
    test_that("single absorbance data loads", {
      expect_s3_class(abs_read(list.files(system.file("extdata", package = "eemanalyzeR"),full.names=TRUE, pattern="ABS")[1]),
                  "data.frame")
    })

    test_that("all absorbance data loads", {
      expect_s3_class(abs_dir_read(system.file("extdata", package = "eemanalyzeR")),"data.frame")
    })

    test_that("eems data throws warning", {
      expect_warning(abs_dir_read(system.file("extdata", package = "eemanalyzeR"), skip=NULL), "Unable to import file")
    })

    test_that("abs pattern selection loading works", {
      expect_equal(dim(abs_dir_read(system.file("extdata", package = "eemanalyzeR"), pattern = "Abs")), c(188,2))
    })

    test_that("abs skip exclusion loading works", {
      expect_equal(dim(abs_dir_read(system.file("extdata", package = "eemanalyzeR"), skip = "manual|BEM|SEM")), c(32,4))
    })
