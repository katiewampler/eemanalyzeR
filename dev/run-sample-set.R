date <- "2025_11_14"
dir <- file.path(fs::path_home(), "Documents/Projects/WWS-Node1-SSFLAT-storm-sampling-flat-fire/Data/water-quality/DOM-EEMs")
input_dir <- file.path(dir, date)

  abs <- abs_dir_read(input_dir)
  eems <- eem_dir_read(input_dir)
  meta <- meta_read(input_dir)
  abs <- add_metadata(meta, abs)
  eems <- add_metadata(meta, eems)
  eems <- add_blanks(eems)

  #processing steps
  abs <- correct_dilution(abs)
  eems <- process_eem(eems, abs, width=c(16,3,20,15))
  eem_plots <- plot(eems, remove_lower = T)
  abs_plots <- plot(abs)
  indices <- get_indices(eems, abs, return="wide", tolerance =0.3)
  abs_index <- indices$abs_index
  eem_index <- indices$eem_index

  export_data(eems, abs, date, file.path(dir, date), meta, indices, eem_plots, abs_plots)


  #code when blank needs to be replaced (incorporate into the validate blanks??)
  abs <- add_metadata(meta, abs)
  eems <- add_metadata(meta, eems)

  eems <- subset_samples(eems, "sample", "SEM", keep=TRUE)
  blk <- subset_samples(eems, "meta_name","BLK2511121", keep=TRUE)
  eems <- subset_samples(eems, "meta_name","BLK2511121")

  eems <- add_blanks(eems, blk, validate = T)


