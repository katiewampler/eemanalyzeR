# testing functions with real data. these are data sets that previously caused issues in the fewsdom code
  #this data won't be included in the package because:
      #(1) the files are large
      #(2) we don't own most of the data

  #this testing is helpful because:
      #(1) it tests different file naming structures
      #(2) it tests different sizes/shapes of files
  library(here)

  #get mdls
    create_mdl("data-raw/long-term-standards/blanks", recursive = TRUE, type="eem")
    create_mdl("data-raw/long-term-standards/blanks", recursive = TRUE, type="abs", pattern="Abs")

    create_tea_std("data-raw/long-term-standards/tea_standards", recursive = TRUE, type="eem")
    create_tea_std("data-raw/long-term-standards/tea_standards", recursive = TRUE, type="abs", pattern="Abs")

  #example 1: Hohner-Lab-2024-07-29
    input_dir <- here("dev/dev-examples/Hohner-Lab-2024-07-29")
    abs <- abs_dir_read(input_dir)
    eems <- eem_dir_read(input_dir)
    meta <- meta_read(input_dir)
    abs <- add_metadata(meta, abs)
    eems <- add_metadata(meta, eems)
    eems <- add_blanks(eems, validate = F)
      #processing steps
      abs <- correct_dilution(abs)
      eems <- process_eem(eems, abs, width=c(16,3,20,15))
      plots <- plot_eem(eems, remove_lower = T)
      indices <- get_indices(eems, abs, return="wide",  mdl_dir = tempdir())
      abs_index <- indices$abs_index
      eem_index <- indices$eem_index

      indices <- get_indices(eems, abs, return="wide", index_method = "usgs",  mdl_dir = tempdir())
      abs_index <- indices$abs_index
      eem_index <- indices$eem_index

      indices <- get_indices(eems, abs, return="wide", index_method = "eemR",  mdl_dir = tempdir())
      abs_index <- indices$abs_index
      eem_index <- indices$eem_index

      export_data(eems, abs, "devtest1", tempdir(),meta, indices, plots)

    #example 2: Hohner-Lab-2025-01-08
    input_dir <- here("dev/dev-examples/Hohner-Lab-2025-01-08")
    abs <- abs_dir_read(input_dir)
    eems <- eem_dir_read(input_dir)
    #meta <- meta_read(input_dir, name="bad_meta.csv") #missing RSU so throws an error as it should [make two copies on with RSU to continue to test dataset]
    meta <- meta_read(input_dir, name="good_meta.csv", validate = F) #this has RSU so it should be fine
    abs <- add_metadata(meta, abs)
    eems <- add_metadata(meta, eems)
    eems <- add_blanks(eems, validate = F)
      #processing steps
      abs <- correct_dilution(abs)
      eems <- process_eem(eems, abs, width=c(16,3,20,15))
      plots <- plot_eem(eems, remove_lower = T)
      indices <- get_indices(eems, abs, return="wide", mdl_dir = tempdir())
      abs_index <- indices$abs_index
      eem_index <- indices$eem_index

      indices <- get_indices(eems, abs, return="wide", index_method = "usgs", mdl_dir = tempdir())
      abs_index <- indices$abs_index
      eem_index <- indices$eem_index

      indices <- get_indices(eems, abs, return="wide", index_method = "eemR", mdl_dir = tempdir())
      abs_index <- indices$abs_index
      eem_index <- indices$eem_index

      export_data(eems, abs, "devtest2", tempdir(), meta, indices, plots)

  #example 3: PNNL-2022-11-10
    input_dir <- here("dev/dev-examples/PNNL-2022-11-10")
    #abs <- abs_dir_read(input_dir) # like this, we get warnings, but still loads
    abs <- abs_dir_read(input_dir, pattern="Abs") #like this we don't get warnings
    eems <- eem_dir_read(input_dir)
    meta <- meta_read(input_dir)
    abs <- add_metadata(meta, abs)
    eems <- add_metadata(meta, eems)
    #eems <- add_blanks(eems) #throws an error because pattern is different
    eems <- add_blanks(eems, pattern="blank$", validate = F)
      #processing steps
      abs <- correct_dilution(abs)
      eems <- process_eem(eems, abs, width=c(16,3,20,15))
      plots <- plot_eem(eems, remove_lower = T)
      indices <- get_indices(eems, abs, return="wide", mdl_dir = tempdir())
      abs_index <- indices$abs_index
      eem_index <- indices$eem_index

      indices <- get_indices(eems, abs, return="wide", index_method = "usgs", mdl_dir = tempdir())
      abs_index <- indices$abs_index
      eem_index <- indices$eem_index

      indices <- get_indices(eems, abs, return="wide", index_method = "eemR", mdl_dir = tempdir())
      abs_index <- indices$abs_index
      eem_index <- indices$eem_index

      export_data(eems, abs, "devtest3", tempdir(), meta, indices, plots)

  #example 4: Vick-Majors-Lab-2024-11-04
    input_dir <- here("dev/dev-examples/Vick-Majors-Lab-2024-11-04")
    abs <- abs_dir_read(input_dir)
    eems <- eem_dir_read(input_dir)
    meta <- meta_read(input_dir)
    abs <- add_metadata(meta, abs)
    eems <- add_metadata(meta, eems)
    eems <- add_blanks(eems, validate = F)
      #processing steps
      abs <- correct_dilution(abs)
      eems <- process_eem(eems, abs, width=c(16,3,20,15))
      plots <- plot_eem(eems, remove_lower = T)
      indices <- get_indices(eems, abs, return="wide", mdl_dir = tempdir())
      abs_index <- indices$abs_index
      eem_index <- indices$eem_index

      indices <- get_indices(eems, abs, return="wide", index_method = "usgs", mdl_dir = tempdir())
      abs_index <- indices$abs_index
      eem_index <- indices$eem_index

      indices <- get_indices(eems, abs, return="wide", index_method = "eemR", mdl_dir = tempdir())
      abs_index <- indices$abs_index
      eem_index <- indices$eem_index

      export_data(eems, abs, "devtest4", tempdir(), meta, indices, plots)

  #example 5: Bladon-Lab-2024-08-19
    input_dir <- here("dev/dev-examples/Bladon-Lab-2024-08-19")
    abs <- abs_dir_read(input_dir)
    eems <- eem_dir_read(input_dir)
    meta <- meta_read(input_dir)
    abs <- add_metadata(meta, abs)
    eems <- add_metadata(meta, eems)
    eems <- add_blanks(eems, validate = F)
      #processing steps
      abs <- correct_dilution(abs)
      eems <- process_eem(eems, abs, width=c(16,3,20,15))
      plots <- plot_eem(eems, remove_lower = T)
      indices <- get_indices(eems, abs, return="wide")
      abs_index <- indices$abs_index
      eem_index <- indices$eem_index

      indices <- get_indices(eems, abs, return="wide", index_method = "usgs")
      abs_index <- indices$abs_index
      eem_index <- indices$eem_index

      indices <- get_indices(eems, abs, return="wide", index_method = "eemR")
      abs_index <- indices$abs_index
      eem_index <- indices$eem_index

      export_data(eems, abs, "devtest5", tempdir(), meta, indices, plots)

  #example 6: Bladon-Lab-2024-08-22
    input_dir <- here("dev/dev-examples/Bladon-Lab-2024-08-22")
    abs <- abs_dir_read(input_dir)
    eems <- eem_dir_read(input_dir)
    meta <- meta_read(input_dir)
    abs <- add_metadata(meta, abs)
    eems <- add_metadata(meta, eems)
    eems <- add_blanks(eems, validate = F)
      #processing steps
      abs <- correct_dilution(abs)
      eems <- process_eem(eems, abs, width=c(16,3,20,15))
      plots <- plot_eem(eems, remove_lower = T)
      indices <- get_indices(eems, abs, return="wide")
      abs_index <- indices$abs_index
      eem_index <- indices$eem_index

      indices <- get_indices(eems, abs, return="wide", index_method = "usgs")
      abs_index <- indices$abs_index
      eem_index <- indices$eem_index

      indices <- get_indices(eems, abs, return="wide", index_method = "eemR")
      abs_index <- indices$abs_index
      eem_index <- indices$eem_index

      export_data(eems, abs, "devtest6", tempdir(), meta, indices, plots)

  #example 7: Bladon-Lab-2024-11-01
    input_dir <- here("dev/dev-examples/Bladon-Lab-2024-11-01")
    abs <- abs_dir_read(input_dir)
    eems <- eem_dir_read(input_dir)
    meta <- meta_read(input_dir)
    abs <- add_metadata(meta, abs)
    eems <- add_metadata(meta, eems)
    eems <- add_blanks(eems, validate = F)
      #processing steps
      abs <- correct_dilution(abs)
      eems <- process_eem(eems, abs, width=c(16,3,20,15))
      plots <- plot_eem(eems, remove_lower = T)
      indices <- get_indices(eems, abs, return = "wide")
      abs_index <- indices$abs_index
      eem_index <- indices$eem_index

      indices <- get_indices(eems, abs, return="wide", index_method = "usgs")
      abs_index <- indices$abs_index
      eem_index <- indices$eem_index

      indices <- get_indices(eems, abs, return="wide", index_method = "eemR")
      abs_index <- indices$abs_index
      eem_index <- indices$eem_index
      export_data(eems, abs, "devtest7", tempdir(), meta, indices, plots)


