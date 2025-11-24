# Test loading absorbance data
    test_that("single absorbance data loads", {
      expect_s3_class(abs_read(list.files(system.file("extdata", package = "eemanalyzeR"),full.names=TRUE, pattern="ABS|Abs")[1]),
                  "abs")
    })

    test_that("manual absorbance gets read in correctly", {
      abs <- abs_read(list.files(system.file("extdata", package = "eemanalyzeR"),full.names=TRUE, pattern="AbsSpectraGraphs")[1])
      expect_s3_class(abs ,"abs")
      expect_equal(abs$data[1,2], 0.0002680016, tolerance = 1e-7)
    })

    test_that("abs class initializes", {
      abs <- abs_read(list.files(system.file("extdata", package = "eemanalyzeR"),full.names=TRUE, pattern="ABS")[1])
      expect_equal(attr(abs, "names"), c("file", "sample", "n", "data","location"))
      expect_equal(dim(abs$data), c(32, 2))

    })
