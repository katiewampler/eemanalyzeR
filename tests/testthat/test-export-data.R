test_that("data export works", {
  #create directory to write to
    dir <- withr::local_tempfile()
    prjname <- basename(dir)
    dir <- dirname(dir)

    #TODO: change to package environment
    #remove readme if it exists because new dataset
    if(exists("readme")){rm("readme", envir = .GlobalEnv)}

  #do processing (manually so we can check the readme)
    eemlist <- add_metadata(metadata,example_eems)
    abslist <- add_metadata(metadata, example_abs)
    eemlist <- add_blanks(eemlist, validate=FALSE)
    expect_warning(eemlist <- process_eem(eemlist, abslist), "trimmed EEM's to match absorbance data wavelengths")

  #test exporting minimum
    export_data(eemlist, abslist,prjname, dir, metadata)

    files <- paste0(paste0(c("readme_", "processed_data_"), prjname), c(".txt", ".rds"))
    expect_true(all(file.exists(file.path(dir, prjname, files))))

    readme_file <- file.path(dir, prjname, files[1])
    expect_equal(length(readLines(readme_file)), 30) #not 40 because here the indices haven't been written
    data <- readRDS(file.path(dir, prjname, files[2]))
    expect_equal(names(data), c("eemlist","abslist","readme","metadata","indices","plots"))
    expect_s3_class(data$metadata, "data.frame")

  #check indices, and plots
    plots <- plot_eem(eemlist, remove_lower = T)
    indices <- get_indices(eemlist, abslist, return="wide",  mdl_dir = system.file("extdata", package = "eemanalyzeR"))

    export_data(eemlist, abslist,prjname, dir, metadata, indices, plots)

    files <- paste0(c(paste0(c("absindices_", "fluorindices_", "summary_plots_"), prjname),
                      c("B1S1ExampleBlankSEM", "B1S2ExampleTeaStdSEM", "B1S3ExampleSampleSEM", "ManualExampleTeaWaterfallPlotSample")), c(rep(".csv",2), rep(".png", 5)))
    expect_true(all(file.exists(file.path(dir, prjname, files))))

    abs_index <- read.csv(file.path(dir, prjname, files[1]))
    expect_true(all(!is.na(abs_index)))
    expect_equal(dim(abs_index), c(4, 10))

    eem_index <- read.csv(file.path(dir, prjname, files[2]))
    expect_true(all(!is.na(eem_index)))
    expect_equal(dim(eem_index), c(4, 27))

    expect_equal(length(readLines(readme_file)), 44) #now 44 because here the indices have been written


  #check writing to csv
    export_data(eemlist, abslist, prjname, dir, metadata, indices, plots, csv = TRUE)
    files <- c(paste0(c("B1S1ExampleBlankSEM", "B1S2ExampleTeaStdSEM", "B1S3ExampleSampleSEM", "ManualExampleTeaWaterfallPlotSample"), "_processed.csv"),
                paste0("absorbance_processed_", prjname, ".csv"))

    expect_true(all(file.exists(file.path(dir, prjname, files))))

    abs_data <- read.csv(file.path(dir, prjname, paste0("absorbance_processed_", prjname, ".csv")))
    eem_data <- read.csv(file.path(dir, prjname, "B1S2ExampleTeaStdSEM_processed.csv"))

    expect_equal(dim(abs_data), c(32,5))
    expect_equal(dim(eem_data), c(26,12))

    expect_equal(colnames(abs_data), c("wavelength", get_sample_info(abslist, "sample")))
    expect_equal(as.vector(as.matrix(eem_data[-1])), as.vector(eemlist[[2]]$x))
  
  

})
