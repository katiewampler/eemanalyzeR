# Tests for meta checking function
test_that("missing columns are caught",{
  meta <- suppressMessages(meta_read(system.file("extdata", package = "eemanalyzeR")))

  #check for missing columns
  expect_error(meta_check(meta[,-which(colnames(meta) == "data_identifier")]), "missing required column")
  expect_error(meta_check(meta[,-which(colnames(meta) == "replicate_no")]), "missing required column")
  expect_error(meta_check(meta[,-which(colnames(meta) == "integration_time_s")]), "missing required column")
#  expect_error(meta_check(meta[,-which(colnames(meta) == "run_type")]), "missing required column")
  expect_error(meta_check(meta[,-which(colnames(meta) == "RSU_area_1s")]), "missing required column")
  expect_error(meta_check(meta[,-which(colnames(meta) == "dilution")]), "missing required column")

})

test_that("columns are made correct type",{
  meta <- suppressMessages(meta_read(system.file("extdata", package = "eemanalyzeR")))
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
  meta <- suppressMessages(meta_read(system.file("extdata", package = "eemanalyzeR")))

  #if one is missing
  meta$data_identifier[-2] <- NA
  expect_error(meta_check(meta), "missing data identifiers for one or more samples")

  #if all are missing
  meta$data_identifier <- NA
  expect_error(meta_check(meta), "missing data identifiers for one or more samples")

})

test_that("missing rsu area is caught",{
  meta <- suppressMessages(meta_read(system.file("extdata", package = "eemanalyzeR")))

  #if one is missing
  meta$RSU_area_1s[-2] <- NA
  expect_error(meta_check(meta), "missing values for RSU adjust area")

  #if all are missing
  meta$RSU_area_1s <- NA
  expect_error(meta_check(meta), "missing values for RSU adjust area")

})

test_that("duplicated ID's are caught",{
  meta <- suppressMessages(meta_read(system.file("extdata", package = "eemanalyzeR")))
  meta <- rbind(meta, meta[1,])

  expect_error(meta_check(meta), "duplicate samples found with the same data_identifier")
})

test_that("dilutions get corrected",{
  meta <- suppressMessages(meta_read(system.file("extdata", package = "eemanalyzeR")))

  meta$dilution <- 0
  expect_warning(col <- meta_check(meta)$dilution, "dilutions were missing or set to 0")
  expect_equal(col, c(1,1,1,1))

  meta$dilution <- NA
  expect_warning(col <- meta_check(meta)$dilution, "dilutions were missing or set to 0")
  expect_equal(col, c(1,1,1,1))

})

test_that("replicate numbers get corrected",{
  meta <- suppressMessages(meta_read(system.file("extdata", package = "eemanalyzeR")))

  meta$replicate_no <- NA
  expect_warning(col <- meta_check(meta)$replicate_no, "replicate numbers were missing")
  expect_equal(col, c(1,1,1,1))

})

# Removed this test now that run_type is optional
# test_that("run type gets flagged",{
#   meta <- suppressMessages(meta_read(system.file("extdata", package = "eemanalyzeR")))
#   meta$run_type <- "wrong_type"

#   expect_error(meta_check(meta), "'run_type' must be either")

#   # sampleQ and Manual must be case sensitive as of 2025-11-19
#   meta$run_type <- "sampleq"
#   expect_error(meta_check(meta), "'run_type' must be either 'sampleQ' or 'manual'")

#   meta$run_type <- "MANUAL"
#   expect_error(meta_check(meta), "'run_type' must be either 'sampleQ' or 'manual'")

# })
