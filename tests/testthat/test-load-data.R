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

    test_that("abs selection loading works", {
      expect_equal(dim(abs_dir_read(system.file("extdata", package = "eemanalyzeR"), pattern = "Abs")), c(188,2))
      expect_equal(dim(abs_dir_read(system.file("extdata", package = "eemanalyzeR"), skip = "manual|BEM|SEM")), c(32,4))
    })

    test_that("column naming works for abs", {
      expect_equal(colnames(abs_dir_read(system.file("extdata", package = "eemanalyzeR"))),
                            c("wavelength", "B1S1ExampleBlankABS","B1S2ExampleTeaStdABS",
                              "B1S3ExampleSampleABS","ExampleAbsManual-Abs_Spectra_Graphs"))
    })

#testing loading metadata
    test_that("metadata loads", {
      expect_s3_class(meta_read(system.file("extdata", package = "eemanalyzeR")), "data.frame")
      expect_error(meta_read(system.file("extdata", package = "eemanalyzeR"), name="wrong_name"), "unable to locate metadata")
    })

    test_that("missing columns are caught",{
      meta <- meta_read(system.file("extdata", package = "eemanalyzeR"))

      #check for missing columns
      expect_error(meta_check(meta[,-which(colnames(meta) == "data_identifier")]), "missing required column")
      expect_error(meta_check(meta[,-which(colnames(meta) == "replicate_no")]), "missing required column")
      expect_error(meta_check(meta[,-which(colnames(meta) == "integration_time_s")]), "missing required column")
      expect_error(meta_check(meta[,-which(colnames(meta) == "run_type")]), "missing required column")
      expect_error(meta_check(meta[,-which(colnames(meta) == "RSU_area_1s")]), "missing required column")
      expect_error(meta_check(meta[,-which(colnames(meta) == "dilution")]), "missing required column")

    })

    test_that("columns are made numeric",{
      meta <- meta_read(system.file("extdata", package = "eemanalyzeR"))
      meta <- meta %>% dplyr::mutate(dplyr::across(dplyr::any_of(c("integration_time_s","RSU_area_1s", "dilution", "DOC_mg_L")), as.character))
      meta <- meta_check(meta)
      #check that columns are converted to numeric
      expect_true(is.numeric(meta$integration_time_s))
      expect_true(is.numeric(meta$RSU_area_1s))
      expect_true(is.numeric(meta$dilution))
      expect_true(is.numeric(meta$DOC_mg_L))
    })

  test_that("missing data identifiers are caught",{
      meta <- meta_read(system.file("extdata", package = "eemanalyzeR"))

      #if one is missing
      meta$data_identifier[-2] <- NA
      expect_error(meta_check(meta), "missing data identifiers for one or more samples")

      #if all are missing
      meta$data_identifier <- NA
      expect_error(meta_check(meta), "missing data identifiers for one or more samples")

    })

  test_that("missing rsu area is caught",{
    meta <- meta_read(system.file("extdata", package = "eemanalyzeR"))

    #if one is missing
    meta$RSU_area_1s[-2] <- NA
    expect_error(meta_check(meta), "missing values for RSU adjust area")

    #if all are missing
    meta$RSU_area_1s <- NA
    expect_error(meta_check(meta), "missing values for RSU adjust area")

  })

  test_that("duplicated ID's are caught",{
    meta <- meta_read(system.file("extdata", package = "eemanalyzeR"))
    meta <- rbind(meta, meta[1,])

    expect_error(meta_check(meta), "duplicate samples found with the same data_identifier")
  })

  test_that("dilutions get corrected",{
    meta <- meta_read(system.file("extdata", package = "eemanalyzeR"))

    meta$dilution <- 0
    expect_warning(col <- meta_check(meta)$dilution, "dilutions were missing or set to 0")
    expect_equal(col, c(1,1,1))

    meta$dilution <- NA
    expect_warning(col <- meta_check(meta)$dilution, "dilutions were missing or set to 0")
    expect_equal(col, c(1,1,1))

  })

  test_that("replicate numbers get corrected",{
    meta <- meta_read(system.file("extdata", package = "eemanalyzeR"))

    meta$replicate_no <- NA
    expect_warning(col <- meta_check(meta)$replicate_no, "replicate numbers were missing")
    expect_equal(col, c(1,1,1))

  })

  test_that("sample type gets flagged",{
    meta <- meta_read(system.file("extdata", package = "eemanalyzeR"))
    meta$run_type <- "wrong_type"

    expect_error(meta_check(meta), "'run_type' must be either")

    meta$run_type <- "sampleq"
    expect_no_error(meta_check(meta))

    meta$run_type <- "MANUAL"
    expect_no_error(meta_check(meta))

  })
