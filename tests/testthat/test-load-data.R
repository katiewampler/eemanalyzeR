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

