dir <- "C:/Users/wampleka/Downloads/2025_11_14"
save <- ""
  abs <- abs_dir_read(input_dir)
  eems <- eem_dir_read(input_dir)
  meta <- meta_read(input_dir)
  abs <- add_metadata(meta, abs)
  eems <- add_metadata(meta, eems)
  eems <- add_blanks(eems)

  #processing steps
  abs <- correct_dilution(abs)
  eems <- process_eem(eems, abs, width=c(16,3,20,15))
  plot_eem(eems, remove_lower = T)
  indices <- get_indices(eems, abs, return="wide")
  abs_index <- indices$abs_index
  eem_index <- indices$eem_index

  export_data(eems, abs, "devtest5", tempdir(),meta, indices, plots)
