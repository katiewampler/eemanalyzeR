##
#' Takes raw EEMs and absorbance data from Aqualog, returns cleaned, processed data
#'
#' @param prjpath path to folder with raw samples
#' @param output_dir TODO
#' @param filename TODO
#' @param interactive TODO
#' @param ...
#' 
#' 
#' @return saves processed EEMs as .csv and .rds files, absorbance as .csv and .rds, and metadata as .csv
#' creates plots for each sample and calculates indices.
#'
#' @details The following steps are chosen by default:
#'
#'  -The file directory is created, raw files are zipped
#'
#'  -A process file is created, The instrument blank is subtracted from the
#'  samples, raman scattering is removed and interpolated, rayleigh scattering
#'  is removed but not interpolated, widths are determined automatically,
#'  samples are corrected for inner filter effects, raman normalized, and
#'  normalized for DOC, excitation is clipped from 247 to 450 nm, emission
#'  is clipped from 247 to 550 nm
#'
#'  -Sample are plotted individually and a summary plot using the parula
#'  color palette with descriptions for the plot titles, the peaks are not
#'  annotated on the plot. Plots are created for DOC normalized data (_DOC),
#'  and for non normalized data.
#'
#'  -Fluorescence indices are reported for data that has and hasn't been DOC
#'  normalized, sample are reported in rows.
#'
#'  If you want to change these defaults add the appropriate arguments into the
#'  function to change the defaults for the processing functions.
#'
#' @export
#'
run_eems <- function(


  # REQUIRED ARGUMENTS (bare minimum)
  prjpath,
  output_dir,
  filename,
  interactive = TRUE,
  ...
    )

{
  # Apply the required arguments to the package environment
  # Option two: load the package environment here?

  # TODO Apply the varargs to some environment (or data structure) only in the
  # function scope so these aren't stored elsewhere.
  varargs <- rlang::list2(...)
  # Pick out the varargs that match the package env names
  parameters_to_modify <- varargs[which(names(varargs) %in% names(.pkgenv))]

  # Clone the package environment (does this work?)
  .fnenv <- rlang::env_clone(.pkgenv, parent = rlang::caller_env())

  # Modify the function environment processing parameters with any from varargs
  modify_config(!!!parameters_to_modify, env = .fnenv)

  # Decide whether the script is running in interactive or batch mode
  rlang::local_interactive(value = interactive)

  # Following processing steps follow the flowchart from github ----------

  # Read the Absorbance data
  abs <- abs_dir_read(prjpath,
                      pattern = get_abs_pattern(.fnenv),
                      skip = get_abs_skip(.fnenv),
                      file_ext = get_abs_file_ext(.fnenv),
                      recursive = get_abs_recurse_read(.fnenv))
  # Read the EEMs data
  eems <- eem_dir_read(prjpath,
                       pattern = get_eem_pattern(.fnenv),
                       skip = get_eem_skip(.fnenv),
                       file_ext = get_eem_file_ext(.fnenv),
                       recursive = get_eem_recurse_read(.fnenv),
                       import_function = get_eem_import_func(.fnenv))
  # TODO don't warn user about overwriting a blank readme since eem_dir_read will overwrite tha abs_dir_read readme

  # TODO maybe meta_file should be default over prjpath
  metadata <- meta_read(prjpath,
                        sheet = get_meta_sheet(.fnenv),
                        validate_metadata = get_meta_validate(.fnenv))
  # Add metadata
  eems <- add_metadata(metadata,
                       eems,
                       sample_type_regex = get_sample_type_regex(.fnenv))
  abs <- add_metadata(metadata,
                      abs,
                      sample_type_regex = get_sample_type_regex(.fnenv))

  # Add blanks
  blanklist <- unique(subset_type(eems, c('iblank')))
  # TODO Need to change how we deal with blanks here. Problems:
  # if you try to subset by iblank and sblank the blanklist won't match the eemlist
  # if you try only to do the iblank then it doesn't allow you to pick the next sblank
  # when the iblank is bad
  # NOTE: rlang::is_interactive might not work great with box large file storage
  eems <- add_blanks(eems,
                     blanklist = blanklist, #TODO FIX
                     validate = rlang::is_interactive())

  # Correct the eems and absorbance for dilutions
  eems <- correct_dilution(eems)
  processed_abs <-  correct_dilution(abs)

  # Process the EEMs
    # subtract_blank
    # remove_scattering
    # ife_correct
    # raman_normalize
    # correct_dilution
    # eem_cut

  # TODO print a message that processing is happening for user
  # collect parameters for readme, and to put into the following functions
  # pars <- rlang::enquos(
  #   ex_clip, em_clip, type, width,
  #   interpolate, method,
  #   cores, cuvle
  # )
  # names(pars) <- c("ex_clip", "em_clip", "type", "width", "interpolate", "method", "cores", "cuvle")

  # Subtract the Blank
  eems <- subtract_blank(eem = eems)
  # Remove Scattering
  eems <- remove_scattering(
    eemlist = eems,
    type = get_type(.fnenv),
    width = get_width(.fnenv),
    interpolate = get_interpolate(.fnenv),
    method = get_method(.fnenv),
    cores = get_cores(.fnenv)
  )
  # Correct for inner filter effects
  eems <- ife_correct(eemlist = eems, 
    processed_abs, 
    cuvle = get_cuvle(.fnenv))
  # Normalize by Raman area
  eems <- raman_normalize(eemlist = eems)
  # Correct for any dilutions
  eems <- correct_dilution(x = eems)

  # Clip EEMs to just the region you care about
  ex_rm <- unique(get_sample_info(eems, "ex")[get_sample_info(eems, "ex") < get_ex_clip(.fnenv)[1] | get_sample_info(eems, "ex") > get_ex_clip(.fnenv)[2]])
  em_rm <- unique(get_sample_info(eems, "em")[get_sample_info(eems, "em") < get_em_clip(.fnenv)[1] | get_sample_info(eems, "em") > get_em_clip(.fnenv)[2]])
  processed_eems <- eemR::eem_cut(eems, ex = ex_rm, em = em_rm, exact = TRUE)
  .write_readme_line("EEMs data were cropped using the 'eemR::eem_cut' function", 
                     "eem_cut", 
                     args = list(ex_clip = get_ex_clip(.fnenv),
                                 em_clip = get_em_clip(.fnenv)))

  # Or we could do it all in one step
  # processed_eems <- process_eem(eems, processed_abs,
  #                               # Default Argument values
  #                               ex_clip = get_ex_clip(.fnenv),
  #                               em_clip = get_em_clip(.fnenv),
  #                               type = get_type(.fnenv),
  #                               width = get_width(.fnenv),
  #                               interpolate = get_interpolate(.fnenv),
  #                               method = get_method(.fnenv),
  #                               cores = get_cores(.fnenv),
  #                               cuvle = get_cuvle(.fnenv))

  # TODOS below:
  # Dev examples code will create mdl files on my computer
  # Validation checks on Processed EEMs and Absorbance Data? ----

  # Report the data ----

  # create plots
  processed_eems_plots <- plot(processed_eems)
  processed_abs_plots <- plot(processed_abs)

  # Save indices
  indices <- get_indices(processed_eems,
                         processed_abs,
                         # Defaults for get_indices
                         # TODO these into wrapper/pkg_env
                         index_method = get_index_method(.fnenv),
                         tolerance = get_tolerance(.fnenv),
                         return = get_return(.fnenv),
                         cuvle = get_cuvle(.fnenv),
                         qaqc_dir = get_qaqc_dir(.fnenv))

  # Save Raw Files
  save_raw_file_status <- export_data(processed_eems,
                                      processed_abs,
                                      filename,
                                      output_dir = get_output_dir(.fnenv), # TODO change to run_eems argument
                                      meta = metadata,
                                      indices = indices,
                                      eem_plot = processed_eems_plots,
                                      abs_plot = processed_abs_plots,
                                      csv = get_csv(.fnenv))
  # TODO check raw file status after added to export-data

  # Done!

}
