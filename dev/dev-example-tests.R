# testing functions with real data. these are data sets that previously caused issues in the fewsdom code
  #this data won't be included in the package because:
      #(1) the files are large
      #(2) we don't own most of the data

  #this testing is helpful because:
      #(1) it tests different file naming structures
      #(2) it tests different sizes/shapes of files
  library(here)


  #example 1: Hohner-Lab-2024-07-29
    input_dir <- here("dev/dev-examples/Hohner-Lab-2024-07-29")
    abs <- abs_dir_read(input_dir)
    eems <- eem_dir_read(input_dir)
    meta <- meta_read(input_dir)
    abs <- add_metadata(meta, abs)
    eems <- add_metadata(meta, eems)
    eems <- add_blanks(eems)
      #processing steps
      eems <- blk_subtract(eems)
      eems <- remove_scattering(eems)
      eems <- ife_correct(eems, abs)
      eems <- raman_normalize(eems)
      eems <- correct_dilution(eems)
      staRdom::ggeem(eems[[7]])

  #example 2: Hohner-Lab-2025-01-08
    input_dir <- here("dev/dev-examples/Hohner-Lab-2025-01-08")
    abs <- abs_dir_read(input_dir)
    eems <- eem_dir_read(input_dir)
    #meta <- meta_read(input_dir, name="bad_meta.csv") #missing RSU so throws an error as it should [make two copies on with RSU to continue to test dataset]
    meta <- meta_read(input_dir, name="good_meta.csv", validate = F) #this has RSU so it should be fine
    abs <- add_metadata(meta, abs)
    eems <- add_metadata(meta, eems)
    eems <- add_blanks(eems)
      #processing steps
      eems <- blk_subtract(eems)
      eems <- remove_scattering(eems)
      eems <- ife_correct(eems, abs)
      eems <- raman_normalize(eems)
      eems <- correct_dilution(eems)
      staRdom::ggeem(eems[[7]])

  #example 3: PNNL-2022-11-10
    input_dir <- here("dev/dev-examples/PNNL-2022-11-10")
    #abs <- abs_dir_read(input_dir) # like this, we get warnings, but still loads
    abs <- abs_dir_read(input_dir, pattern="Abs") #like this we don't get warnings
    eems <- eem_dir_read(input_dir)
    meta <- meta_read(input_dir)
    abs <- add_metadata(meta, abs)
    eems <- add_metadata(meta, eems)
    #eems <- add_blanks(eems) #throws an error because pattern is different
    eems <- add_blanks(eems, pattern="blank$")
      #processing steps
      eems <- blk_subtract(eems)
      eems <- remove_scattering(eems)
      eems <- ife_correct(eems, abs)
      eems <- raman_normalize(eems)
      eems <- correct_dilution(eems)
      staRdom::ggeem(eems[[1]])

  #example 4: Vick-Majors-Lab-2024-11-04
    input_dir <- here("dev/dev-examples/Vick-Majors-Lab-2024-11-04")
    abs <- abs_dir_read(input_dir)
    eems <- eem_dir_read(input_dir)
    meta <- meta_read(input_dir)
    abs <- add_metadata(meta, abs)
    eems <- add_metadata(meta, eems)
    eems <- add_blanks(eems)
      #processing steps
      eems <- blk_subtract(eems)
      eems <- remove_scattering(eems)
      eems <- ife_correct(eems, abs)
      eems <- raman_normalize(eems)
      eems <- correct_dilution(eems)
      staRdom::ggeem(eems[[1]])

  #example 5: Bladon-Lab-2024-08-19
    input_dir <- here("dev/dev-examples/Bladon-Lab-2024-08-19")
    abs <- abs_dir_read(input_dir)
    eems <- eem_dir_read(input_dir)
    meta <- meta_read(input_dir)
    abs <- add_metadata(meta, abs)
    eems <- add_metadata(meta, eems)
    eems <- add_blanks(eems)
      #processing steps
      eems <- blk_subtract(eems)
      eems <- remove_scattering(eems)
      eems <- ife_correct(eems, abs)
      eems <- raman_normalize(eems)
      eems <- correct_dilution(eems)
      staRdom::ggeem(eems[[2]])

  #example 6: Bladon-Lab-2024-08-22
    input_dir <- here("dev/dev-examples/Bladon-Lab-2024-08-22")
    abs <- abs_dir_read(input_dir)
    eems <- eem_dir_read(input_dir)
    meta <- meta_read(input_dir)
    abs <- add_metadata(meta, abs)
    eems <- add_metadata(meta, eems)
    eems <- add_blanks(eems)
      #processing steps
      eems <- blk_subtract(eems)
      eems <- remove_scattering(eems)
      eems <- ife_correct(eems, abs)
      eems <- raman_normalize(eems)
      eems <- correct_dilution(eems)
      staRdom::ggeem(eems[[2]])

  #example 7: Bladon-Lab-2024-11-01
    input_dir <- here("dev/dev-examples/Bladon-Lab-2024-11-01")
    abs <- abs_dir_read(input_dir)
    eems <- eem_dir_read(input_dir)
    meta <- meta_read(input_dir)
    abs <- add_metadata(meta, abs)
    eems <- add_metadata(meta, eems)
    eems <- add_blanks(eems)
      #processing steps
      eems <- blk_subtract(eems)
      eems <- remove_scattering(eems)
      eems <- ife_correct(eems, abs)
      eems <- raman_normalize(eems)
      eems <- correct_dilution(eems)
      staRdom::ggeem(eems[[2]])


