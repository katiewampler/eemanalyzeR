date <- "2025_11_14"
dir <- "T:/Research/Wildfire_Water_Security/02_Nodes/01_Empirical/05_Projects/storm-chasing/Flat-Fire/Data/water-quality/DOM-EEMs"
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
  indices <- get_indices(eems, abs, return="wide")
  abs_index <- indices$abs_index
  eem_index <- indices$eem_index

  export_data(eems, abs, date, file.path(dir, date), meta, indices, eem_plots, abs_plots)
