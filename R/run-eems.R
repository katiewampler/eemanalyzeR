#' Load, clean, and process EEMs and absorbance data
#'
#' The main high-level function of the [eemanalyzeR] package.
#' It is designed for users who want a fast, automated workflow without needing
#' to write complex R code or string together a bunch of processing functions.
#' By pointing the function to a folder of raw EEMs and absorbance data,
#' the function performs the full processing pipeline from start to finish.
#'
#' @param input_dir Path to folder containing raw EEMs and/or absorbance files.
#' @param output_dir Path to save the processed data.
#' @param filename A character string, used for file names.
#' @param interactive Logical, used to determine if user input should be used.
#' @param blanklist `eemlist` of blank files to subtract from samples.
#' Automatically uses instrument blanks if not provided.
#' @param qaqc_checks Logical, should user QA/QC files be used to check data?
#' @param ... additional arguments used to make one time modifications to processing
#' arguments and plotting ([plot.eemlist]). List of optional arguments
#' to modify within the processing are located in [default_config]. See details
#' for more info on modifying processing configuration.
#'
#' @note Currently `run_eems` does not allow for custom import or index functions.
#'
#' @return The following files are saved to the output directory specified.
#' The list contains:
#'  - **eemlist:** the `eemlist`
#'  - **abslist:** the `abslist`
#'  - **readme:** a character vector containing information about the data processing steps
#'  - **meta:** the metadata associated with the samples, may be `NULL` if not provided
#'  - **indices:** a list of EEMs and absorbance indices, may be `NULL` if not provided
#'  - **eem_plot:** a list of EEMs plots, may be `NULL` if not provided
#'  - **abs_plot:** a ggplot2 object of the absorbance data, may be `NULL` if not provided

#' @md
#'
#' @details The only requirement to use this wrapper function to process EEMs
#' and absorbance data is an **input directory** containing the raw data files and
#' a metadata spreadsheet. However, there are many optional arguments for the
#' user to modify the EEMs and absorbance processing. The names and default values
#' for these arguments can be found in the documentation for the [default_config].
#'
#' There are four ways for user to use and modify the processing configuration defaults.
#'
#' - **Option 1:** User doesn't change anything, the package defaults are used by `run_eems`.
#'
#' - **Option 2:** User creates a file (stored on their computer) that has processing defaults that
#' eemanalyzeR pulls from at load time. This is created using the [edit_user_config()] function.
#'
#' - **Option 3:** User modifies the defaults *BEFORE* using the `run_eems` function using [modify_config()] function.
#' This modifies the settings for the R session and will be applied to any data processing that occurs until
#' the package is reloaded or the R session is restarted. After the package is reloaded the defaults revert
#' back to the package defaults or (if they exist) user defaults.
#'
#' - **Option 4:** User supplies arguments to `run_eems` function that modify processing *ONLY* during that run. These
#' configuration options will not persist across multiple tries of `run_eems` and must be specified each time,
#' but the argument values will be reported in the readme file.
#'
#' @inherit export_data return
#' @export
#'
run_eems <- function(


  # REQUIRED ARGUMENTS (bare minimum)
  input_dir,
  # Optional arguments
  output_dir = NA,
  filename = NA,
  interactive = TRUE,
  blanklist = NULL,
  qaqc_checks = TRUE,
  ...
    )

{

  # Apply the varargs to function environment only in the
  # function scope so these aren't stored elsewhere.
  varargs <- rlang::list2(...)
  # Pick out the varargs that match the config names
  parameters_to_modify <- varargs[which(names(varargs) %in% names(.pkgenv$config))]
  # Modify the output_dir and filename if it exists in the function call
  if(!missing("output_dir")) parameters_to_modify$output_dir <- output_dir
  if(!missing("filename"))   parameters_to_modify$filename   <- filename

  # Clone the package environment - Everything below should use  .fnenv
  .fnenv <- rlang::env_clone(.pkgenv, parent = rlang::caller_env())

  # Modify the function environment processing parameters with any from varargs
  modify_config(!!!parameters_to_modify, env = .fnenv)

  # Decide whether the script is running in interactive or batch mode
  rlang::local_interactive(value = interactive)

  # Following processing steps follow the flowchart from github ----------
  message("Loading samples...")
  # Read the Absorbance data
  abs <- abs_dir_read(input_dir,
    pattern = get_abs_pattern(.fnenv),
    skip = get_abs_skip(.fnenv),
    file_ext = get_abs_file_ext(.fnenv),
    recursive = get_abs_recurse_read(.fnenv),
    verbose = FALSE
  )
  # Read the EEMs data
  eems <- eem_dir_read(input_dir,
    pattern = get_eem_pattern(.fnenv),
    skip = get_eem_skip(.fnenv),
    file_ext = get_eem_file_ext(.fnenv),
    recursive = get_eem_recurse_read(.fnenv),
    import_function = get_eem_import_func(.fnenv),
    verbose = FALSE
  )

  # Find the metadata file in the input directory.
  metadata <- meta_read(input_dir,
    meta_file = get_meta_file(.fnenv),
    sheet = get_meta_sheet(.fnenv),
    validate_metadata = get_meta_validate(.fnenv)
  )

  # Add metadata
  eems <- add_metadata(metadata,
    eems,
    iblank_pattern = get_iblank_pattern(.fnenv),
    sblank_pattern = get_sblank_pattern(.fnenv),
    check_pattern = get_check_pattern(.fnenv)
  )
  abs <- add_metadata(metadata,
    abs,
    iblank_pattern = get_iblank_pattern(.fnenv),
    sblank_pattern = get_sblank_pattern(.fnenv),
    check_pattern = get_check_pattern(.fnenv)
  )

  # Add blanks
  # NOTE: rlang::is_interactive might not work great with box large file storage
  # Automatically create blanklist if no blanklist provided
  if (is.null(blanklist)) {
    # separate into blanklist and eemlist based on pattern given
    blanklist <- subset_type(eems, type = c("iblank", "sblank"))
  }
  # First validate blanklist (only if in interactive session)
  if(interactive & get_blank_validate(.fnenv)) {
    blanklist <- validate_blanks(blanklist)
  } else {
    # If we aren't validating then use only the iblank
    blanklist <- subset_type(blanklist, "iblank")
  }

  # Add the correct blanks to the eemlist
    #will remove blanks from eems in the add_blanks function
  eems <- add_blanks(eems, blanklist)

  # Correct the eems and absorbance for dilutions
  eems <- correct_dilution(eems)
  processed_abs <- correct_dilution(abs)

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
  eems <- ife_correct(
    eemlist = eems,
    processed_abs,
    cuvle = get_cuvle(.fnenv)
  )
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
    args = list(
      ex_clip = get_ex_clip(.fnenv),
      em_clip = get_em_clip(.fnenv)
    )
  )
  message("All EEMs and Absorbance data successfully processed. See readme for notes and warnings.")

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

  # Report the data ----
  # create plots
  processed_eems_plots <- plot(processed_eems, ...)
  processed_abs_plots <- plot(processed_abs, ...)
  message("EEMs and absorbance successfully plotted.")

  # Save indices
    #Get QAQC directory
      #if FALSE, will put as NULL with get-indices will know to ignore files
      #if TRUE, will get user_config file value, if NA will use default
      #this value is passed to get-indices
    if(qaqc_checks){
      qaqc_dir <-  get_qaqc_dir(.fnenv)
      qaqc_dir <- ifelse(is.na(qaqc_dir), .qaqc_dir(), qaqc_dir)
    }else{
      qaqc_dir <-  NULL
    }

  # Check MDLS are in the get_indices function
  indices <- get_indices(processed_eems,
    processed_abs,
    # Defaults for get_indices
    index_method = get_index_method(.fnenv),
    tolerance = get_tolerance(.fnenv),
    return = get_return(.fnenv),
    cuvle = get_cuvle(.fnenv),
    qaqc_dir = qaqc_dir
  )
  message("Calculated absorbance and fluorescence indices.")

  # Save Raw Files
  save_raw_file_status <- export_data(processed_eems,
    processed_abs,
    filename = get_filename(.fnenv),
    output_dir = get_output_dir(.fnenv),
    meta = metadata,
    indices = indices,
    eem_plot = processed_eems_plots,
    abs_plot = processed_abs_plots,
    csv = get_csv(.fnenv)
  )

  # Done!
}
