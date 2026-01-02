test_that("data export works", {
  #create directory to write to
    dir <- withr::local_tempfile()
    prjname <- basename(dir)
    dir <- dirname(dir)

  # Set readme back to NULL if it exists because new dataset
  if (!is.null(get_readme())) {
    set_readme(NULL)
    message("NOTE: removed previous 'readme' file")
  }

  #do processing (manually so we can check the readme)
    eemlist <- add_metadata(metadata,example_eems)
    abslist <- add_metadata(metadata, example_abs)
    blanklist <- subset_type(eemlist, "iblank")
    eemlist <- add_blanks(eemlist, blanklist)
    eemlist <- process_eem(eemlist, abslist)

  #test exporting minimum
    expect_message(export_data(eemlist=eemlist,
                               abslist=abslist,
                               filename=prjname,
                               meta=metadata,
                               output_dir = dir), "Data successfully exported to")

    files <- paste0(paste0(c("readme_", "processed_data_"), prjname), c(".txt", ".rds"))
    expect_true(all(file.exists(file.path(dir, files))))

    readme_file <- file.path(dir, files[1])
    expect_equal(length(readLines(readme_file)), 30) #not 40 because here the indices haven't been written
    data <- readRDS(file.path(dir, files[2]))
    expect_equal(names(data), c("eemlist","abslist","readme","metadata","indices","eem_plot", "abs_plot"))
    expect_s3_class(data$metadata, "data.frame")

  #check indices, and plots
    eem_plots <- plot(eemlist, remove_lower = T)
    abs_plots <- plot(abslist)
    indices <- get_indices(eemlist, abslist, return="wide",  qaqc_dir = system.file("extdata", package = "eemanalyzeR"))

    expect_message(export_data(eemlist=eemlist,
                               abslist=abslist,
                               filename=prjname,
                               meta=metadata,
                               indices=indices,
                               eem_plot = eem_plots,
                               abs_plot = abs_plots,
                               output_dir = dir), "Data successfully exported to")

    files <- paste0(c(paste0(c("absindices_", "fluorindices_", "summary_plots_", "absorbance_plot_"), prjname),
                      c("B1S1ExampleBlankSEM", "B1S2ExampleTeaStdSEM", "B1S3ExampleSampleSEM", "ManualExampleTeaWaterfallPlotSample")), c(rep(".csv",2), rep(".png", 6)))
    expect_true(all(file.exists(file.path(dir, files))))

    abs_index <- read.csv(file.path(dir, files[1]))
    expect_true(all(!is.na(abs_index)))
    expect_equal(dim(abs_index), c(4, 10))

    eem_index <- read.csv(file.path(dir, files[2]))
    expect_true(all(!is.na(eem_index)))
    expect_equal(dim(eem_index), c(4, 27))

    expect_equal(length(readLines(readme_file)), 44) #now 44 because here the indices have been written


  #check writing to csv
    expect_message(export_data(eemlist=eemlist,
                               abslist=abslist,
                               filename=prjname,
                               meta=metadata,
                               indices=indices,
                               eem_plot = eem_plots,
                               abs_plot = abs_plots,
                               output_dir = dir, csv = TRUE), "Data successfully exported to")
    files <- c(paste0(c("B1S1ExampleBlankSEM", "B1S2ExampleTeaStdSEM", "B1S3ExampleSampleSEM", "ManualExampleTeaWaterfallPlotSample"), "_processed.csv"),
                paste0("absorbance_processed_", prjname, ".csv"), paste0("metadata_", prjname, ".csv"))

    expect_true(all(file.exists(file.path(dir, files))))

    abs_data <- read.csv(file.path(dir, paste0("absorbance_processed_", prjname, ".csv")))
    eem_data <- read.csv(file.path(dir, "B1S2ExampleTeaStdSEM_processed.csv"))

    expect_equal(dim(abs_data), c(32,5))
    expect_equal(dim(eem_data), c(26,12))

    expect_equal(colnames(abs_data), c("wavelength", get_sample_info(abslist, "sample")))
    expect_equal(as.vector(as.matrix(eem_data[-1])), as.vector(eemlist[[2]]$x))



})
