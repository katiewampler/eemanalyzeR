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

  test_that("columns are made correct type",{
    meta <- meta_read(system.file("extdata", package = "eemanalyzeR"))
    meta <- meta %>% dplyr::mutate(dplyr::across(dplyr::any_of(c("integration_time_s","RSU_area_1s", "dilution", "DOC_mg_L")), as.character))
    meta <- meta_check(meta)
    #check that columns are converted to numeric
    expect_true(is.numeric(meta$integration_time_s))
    expect_true(is.numeric(meta$RSU_area_1s))
    expect_true(is.numeric(meta$dilution))
    expect_true(is.numeric(meta$DOC_mg_L))

    expect_s3_class(meta$analysis_date, "POSIXct") #dates are converted to dates
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

#testing loaded metadata
  test_that("missing samples are warned", {
  meta <- meta_read(system.file("extdata", package = "eemanalyzeR"))
  meta <- rbind(meta, meta[1,])
  meta$data_identifier[4] <- "missing_sample"
  abs <- abs_dir_read(system.file("extdata", package = "eemanalyzeR"))

  expect_warning(abs_add_meta(meta, abs), "the following sample is in metadata but was missing in absorbance data")

  meta <- meta[1,]
  expect_warning(abs_add_meta(meta, abs), "the following absorbance data are missing from metadata")

  eems <- eem_dir_read(system.file("extdata", package = "eemanalyzeR"), pattern = "SEM")
  meta <- meta_read(system.file("extdata", package = "eemanalyzeR"))
  meta <- rbind(meta, meta[1,])
  meta$data_identifier[4] <- "missing_sample"

  expect_warning(eem_add_meta(meta, eems), "the following sample is in metadata but was missing in EEM's data")

  meta <- meta[1,]
  expect_warning(eem_add_meta(meta, eems), "the following EEM's data are missing from metadata")
})


#test that things are added
#test that objects are still abslist and eemlist
