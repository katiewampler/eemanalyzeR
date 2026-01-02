# testing functions with real data. these are data sets that previously caused issues in the fewsdom code
  #this data won't be included in the package because:
      #(1) the files are large
      #(2) we don't own most of the data

  #this testing is helpful because:
      #(1) it tests different file naming structures
      #(2) it tests different sizes/shapes of files
  library(here)

  #get mdls
    create_mdl("data-raw/long-term-standards/blanks", recursive = TRUE, type="eem",
               iblank="_blank")

    # create_mdl doesn't have pattern argument - warns user about mdls
    create_mdl("data-raw/long-term-standards/blanks", recursive = TRUE,
               type="abs", iblank="_blank")

    create_std("data-raw/long-term-standards/tea-standards", recursive = TRUE,
                   type="eem", iblank="_blank", abs_pattern="Abs")

    create_std("data-raw/long-term-standards/tea-standards", recursive = TRUE,
                   type="abs", abs_pattern="Abs")

  #example 1: Hohner-Lab-2024-07-29
    input_dir <- here("dev/dev-examples/Hohner-Lab-2024-07-29")
    abs <- abs_dir_read(input_dir)
    eems <- eem_dir_read(input_dir)
    meta <- meta_read(input_dir)
    abs <- add_metadata(meta, abs)
    eems <- add_metadata(meta, eems)
    blk <- subset_type(eems, "iblank")
    blk <- validate_blanks(blk)
    eems <- add_blanks(eems, blk)
      #processing steps
      abs <- correct_dilution(abs)
      eems <- process_eem(eems, abs, width=c(16,3,20,15))
      plots <- plot(eems, remove_lower = T)
      indices <- get_indices(eems, abs, return="wide",  qaqc_dir = tempdir())
      abs_index <- indices$abs_index
      eem_index <- indices$eem_index

      indices <- get_indices(eems, abs, return="wide", index_method = "usgs",  qaqc_dir = tempdir())
      abs_index <- indices$abs_index
      eem_index <- indices$eem_index

      indices <- get_indices(eems, abs, return="wide", index_method = "eemR",  qaqc_dir = tempdir())
      abs_index <- indices$abs_index
      eem_index <- indices$eem_index

      export_data(eems, abs, "devtest1", meta, indices, plots)

  #try with run_eems function
    run_eems(input_dir, filename="devtest1_auto", qaqc_checks = FALSE)

    #example 2: Hohner-Lab-2025-01-08
    input_dir <- here("dev/dev-examples/Hohner-Lab-2025-01-08")
    abs <- abs_dir_read(input_dir)
    eems <- eem_dir_read(input_dir)
    #meta <- meta_read(input_dir, name="bad_meta.csv") #missing RSU so throws an error as it should [make two copies on with RSU to continue to test dataset]
    meta <- meta_read(input = file.path(input_dir, "good_meta.csv")) #this has RSU so it should be fine
    abs <- add_metadata(meta, abs)
    eems <- add_metadata(meta, eems)
    blk <- subset_type(eems, "iblank")
    blk <- validate_blanks(blk)
    eems <- add_blanks(eems, blk)
      #processing steps
      abs <- correct_dilution(abs)
      eems <- process_eem(eems, abs, width=c(16,3,20,15))
      plots <- plot(eems, remove_lower = T)
      indices <- get_indices(eems, abs, return="wide", qaqc_dir = tempdir())
      abs_index <- indices$abs_index
      eem_index <- indices$eem_index

      indices <- get_indices(eems, abs, return="wide", index_method = "usgs", qaqc_dir = tempdir())
      abs_index <- indices$abs_index
      eem_index <- indices$eem_index

      indices <- get_indices(eems, abs, return="wide", index_method = "eemR", qaqc_dir = tempdir())
      abs_index <- indices$abs_index
      eem_index <- indices$eem_index

      export_data(eems, abs, "devtest2", meta, indices, plots)

      #try with run_eems function
      run_eems(input_dir, filename="devtest2_auto",
               qaqc_checks = FALSE, meta_file = "good_meta.csv")

  #example 3: PNNL-2022-11-10
    input_dir <- here("dev/dev-examples/PNNL-2022-11-10")
    #abs <- abs_dir_read(input_dir) # like this, we get warnings, but still loads
    abs <- abs_dir_read(input_dir, pattern="Abs") #like this we don't get warnings
    eems <- eem_dir_read(input_dir)
    meta <- meta_read(input_dir)
    abs <- add_metadata(meta, abs)
    eems <- add_metadata(meta, eems,
                           iblank_pattern = "blank$",
                           sblank_pattern = "none",
                           check_pattern = "Tea|tea"
                         )
    blk <- subset_type(eems, "iblank")
    blk <- validate_blanks(blk)
    eems <- add_blanks(eems, blk)
      #processing steps
      abs <- correct_dilution(abs)
      eems <- process_eem(eems, abs, width=c(16,3,20,15))
      plots <- plot(eems, remove_lower = T)
      indices <- get_indices(eems, abs, return="wide", qaqc_dir = tempdir())
      abs_index <- indices$abs_index
      eem_index <- indices$eem_index

      indices <- get_indices(eems, abs, return="wide", index_method = "usgs", qaqc_dir = tempdir())
      abs_index <- indices$abs_index
      eem_index <- indices$eem_index

      indices <- get_indices(eems, abs, return="wide", index_method = "eemR", qaqc_dir = tempdir())
      abs_index <- indices$abs_index
      eem_index <- indices$eem_index

      export_data(eems, abs, "devtest3", meta, indices, plots)

      #try with run_eems function
      run_eems(input_dir, abs_pattern="Abs", qaqc_checks = FALSE,
               iblank_pattern = "blank$", filename="devtest3_auto")

  #example 4: Vick-Majors-Lab-2024-11-04
    input_dir <- here("dev/dev-examples/Vick-Majors-Lab-2024-11-04")
    abs <- abs_dir_read(input_dir)
    eems <- eem_dir_read(input_dir)
    meta <- meta_read(input_dir)
    abs <- add_metadata(meta, abs)
    eems <- add_metadata(meta, eems)
    blk <- subset_type(eems, "iblank")
    blk <- validate_blanks(blk)
    eems <- add_blanks(eems, blk)
      #processing steps
      abs <- correct_dilution(abs)
      eems <- process_eem(eems, abs, width=c(16,3,20,15))
      plots <- plot(eems, remove_lower = T)
      indices <- get_indices(eems, abs, return="wide", qaqc_dir = tempdir())
      abs_index <- indices$abs_index
      eem_index <- indices$eem_index

      indices <- get_indices(eems, abs, return="wide", index_method = "usgs", qaqc_dir = tempdir())
      abs_index <- indices$abs_index
      eem_index <- indices$eem_index

      indices <- get_indices(eems, abs, return="wide", index_method = "eemR", qaqc_dir = tempdir())
      abs_index <- indices$abs_index
      eem_index <- indices$eem_index

      export_data(eems, abs, "devtest4", meta, indices, plots)

    #try with run_eems function
      run_eems(input_dir, filename="devtest4_auto", qaqc_checks = FALSE,
               remove_lower = TRUE)

  #example 5: Bladon-Lab-2024-08-19
    input_dir <- here("dev/dev-examples/Bladon-Lab-2024-08-19")
    abs <- abs_dir_read(input_dir)
    eems <- eem_dir_read(input_dir)
    meta <- meta_read(input_dir)
    abs <- add_metadata(meta, abs)
    eems <- add_metadata(meta, eems)
    blk <- subset_type(eems, "iblank")
    blk <- validate_blanks(blk)
    eems <- add_blanks(eems, blk)
      #processing steps
      abs <- correct_dilution(abs)
      eems <- process_eem(eems, abs, width=c(16,3,20,15))
      plots <- plot(eems, remove_lower = T)
      indices <- get_indices(eems, abs, return="wide")
      abs_index <- indices$abs_index
      eem_index <- indices$eem_index

      indices <- get_indices(eems, abs, return="wide", index_method = "usgs")
      abs_index <- indices$abs_index
      eem_index <- indices$eem_index

      indices <- get_indices(eems, abs, return="wide", index_method = "eemR")
      abs_index <- indices$abs_index
      eem_index <- indices$eem_index

      export_data(eems, abs, "devtest5", meta, indices, plots)

    #try with run_eems function
      run_eems(input_dir, filename="devtest5_auto")

  #example 6: Bladon-Lab-2024-08-22
    input_dir <- here("dev/dev-examples/Bladon-Lab-2024-08-22")
    abs <- abs_dir_read(input_dir)
    eems <- eem_dir_read(input_dir)
    meta <- meta_read(input_dir)
    abs <- add_metadata(meta, abs)
    eems <- add_metadata(meta, eems)
    blk <- subset_type(eems, "iblank")
    blk <- validate_blanks(blk)
    eems <- add_blanks(eems, blk)
      #processing steps
      abs <- correct_dilution(abs)
      eems <- process_eem(eems, abs, width=c(16,3,20,15))
      plots <- plot(eems, remove_lower = T)
      indices <- get_indices(eems, abs, return="wide")
      abs_index <- indices$abs_index
      eem_index <- indices$eem_index

      indices <- get_indices(eems, abs, return="wide", index_method = "usgs")
      abs_index <- indices$abs_index
      eem_index <- indices$eem_index

      indices <- get_indices(eems, abs, return="wide", index_method = "eemR")
      abs_index <- indices$abs_index
      eem_index <- indices$eem_index

      export_data(eems, abs, "devtest6", meta, indices, plots)

    #try with run_eems function
      run_eems(input_dir, filename="devtest6_auto")

  #example 7: Bladon-Lab-2024-11-01
    input_dir <- here("dev/dev-examples/Bladon-Lab-2024-11-01")
    abs <- abs_dir_read(input_dir)
    eems <- eem_dir_read(input_dir)
    meta <- meta_read(input_dir)
    abs <- add_metadata(meta, abs)
    eems <- add_metadata(meta, eems)
    blk <- subset_type(eems, "iblank")
    blk <- validate_blanks(blk)
    eems <- add_blanks(eems, blk)
      #processing steps
      abs <- correct_dilution(abs)
      eems <- process_eem(eems, abs, width=c(16,3,20,15))
      plots <- plot(eems, remove_lower = T)
      indices <- get_indices(eems, abs, return = "wide")
      abs_index <- indices$abs_index
      eem_index <- indices$eem_index

      indices <- get_indices(eems, abs, return="wide", index_method = "usgs")
      abs_index <- indices$abs_index
      eem_index <- indices$eem_index

      indices <- get_indices(eems, abs, return="wide", index_method = "eemR")
      abs_index <- indices$abs_index
      eem_index <- indices$eem_index
      export_data(eems, abs, "devtest7", meta, indices, plots)

    #try with run_eems function
      run_eems(input_dir, filename="devtest7_auto")


