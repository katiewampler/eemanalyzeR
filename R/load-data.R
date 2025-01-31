#' Read excitation-emission fluorescence matrices (EEM) from directory
#'
#' A wrapper for \link[eemR]{eem_read} to read all EEMs in a directory even when
#' it contains other files.
#'
#' @param input_dir path to folder containing raw EEMs files
#' @param pattern optional. a character string containing a \code{\link[base]{regular expression}} to be matched to the files in input_dir.
#' only files matching the pattern will be loaded.
#' @param skip a character string containing a \code{\link[base]{regular expression}} to be matched to the files in input_dir.
#' any files matching this string will be ignored. useful for ignoring absorbance data.
#' @param file_ext character. the file extension of the EEMs
#' @param recursive logical. should the function recurse into directories?
#' @param import_function character or a user-defined function to import an EEM. for more details see \link[eemR]{eem_read}

#'
#' @importFrom eemR eem_read
#' @importFrom purrr discard
#' @importFrom magrittr %>%
#'
#' @return object of class eemlist
#' @export
#'
#' @examples
#' # Load all data from directory --------------
#' eem_list <- eem_dir_read(system.file("extdata", package = "eemanalyzeR"))
#'
#' # Load all data that matches a pattern from directory -------------
#' eem_list <- eem_dir_read(system.file("extdata", package = "eemanalyzeR"), pattern="SEM")
#'
eem_dir_read <- function(input_dir, pattern = NULL, skip="(?i)abs", file_ext="dat",
                         recursive = FALSE, import_function="aqualog"){
  stopifnot(dir.exists(input_dir))

  warnings_list <- list()  # Initialize an empty list to store warnings

  #wrapper on eemR::read_eem to try and catch errors from absorbance data being included
    .try_eem_read <- function(file, recursive=F, import_function){
      tryCatch({eem <- eemR::eem_read(file=file, recursive=recursive, import_function = import_function)
        #add additional attributes
        attr(eem[[1]], "is_doc_normalized") <- FALSE
        attr(eem[[1]], "is_dil_corrected") <- FALSE
        return(eem)},
      error = function(e) {
        # Check if it's a specific error
        if (grepl("argument of length 0", conditionMessage(e))) {
          warning("Unable to import file: ", file, ".\nPlease use the 'pattern' and 'skip' arguments to ensure only EEM's files are selected.", call. = FALSE)
          return(NULL)
        } else {
          stop("An unexpected error occurred: ", conditionMessage(e), call. = FALSE)}})
    }

  #get files to read in
    files <- list.files(input_dir, full.names = T, recursive = recursive)

    #only gets files with correct file extension and ones that optionally match the pattern
    ext <- paste0("[.]", file_ext, "$")
      if(is.null(pattern)){
        if(is.null(skip)){
          load_files <- files[grepl(ext, files)]
        }else{
          load_files <- files[grepl(ext, files) & !(grepl(skip, files))]
        }
      }else{
        if(is.null(skip)){
          load_files <- files[grepl(ext, files) & grepl(pattern, files)]
        }else{
          load_files <- files[grepl(ext, files) & grepl(pattern, files) & !(grepl(skip, files))]}
        }

  #read files
    eem_list <- withCallingHandlers(lapply(load_files, .try_eem_read, import_function=import_function),
                                    warning = function(w) {
                                      warnings_list <<- c(warnings_list, conditionMessage(w))  # Add the warning message
                                      invokeRestart("muffleWarning")  # Prevent the warning from printing immediately
                                    })

    # Combine all collected warnings into one [probably unnecessary, I don't think anything else should generate a warning]
    if (length(warnings_list) > 0) {
      if(sum(grepl("Unable to import file: ", warnings_list))>0){
        remove_warn <- grep("Unable to import file: |.\nPlease use the 'pattern' and 'skip' arguments to ensure only EEM's files are selected.", warnings_list)
        error_files <- gsub("Unable to import file: |.\nPlease use the 'pattern' and 'skip' arguments to ensure only EEM's files are selected.", "", warnings_list)
        abs_warning <- paste0("Unable to import file(s):\n", paste(error_files, collapse="\n"),"\nPlease use the 'pattern' and 'skip' arguments to ensure only EEM's files are selected.")
      }
      combined_warning <- paste(warnings_list[-remove_warn], abs_warning, collapse = "\n")
      warning(combined_warning)
    }

    #combine eems
    eem_list <- lapply(eem_list, `[[`, 1)
    eem_list <- eem_list %>% purrr::discard(is.null)
    class(eem_list) <- "eemlist"

    return(eem_list)
}


#' Read a single absorbance file into R
#'
#' Will accept absorbance files run via Aqualog exported using the sample Q or manually. If the function tries
#' to load an EEM's file it will return a warning and a NULL value for absorbance.
#'
#' @param file file path to the absorbance file to load
#' @importFrom stringr str_extract_all
#' @importFrom tools file_ext
#'
#' @return An object of class \code{abs} containing:
#' \itemize{
#'  \item file The filename of the absorbance data.
#'  \item sample The sample name of the absorbance data.
#'  \item n The number of wavelengths absorbance was measured at.
#'  \item data A \code{data.frame} with absorbance data.
#'  \item dilution The dilution factor for the absorbance data.
#'  \item location Directory of the absorbance data.
#' }
#' @export
#'
#' @examples
#' abs_files <- list.files(system.file("extdata", package = "eemanalyzeR"),
#' full.names=TRUE, pattern="ABS")
#' abs <- abs_read(abs_files[1])

abs_read <- function(file){
    #captures warning from trying to rbind eem's data

    .safe_rbind <- function(abs){
      tryCatch({abs <- do.call(rbind, abs)
      return(abs)},
      warning = function(w) {
        # Check if it's a specific error
        if (grepl("number of columns of result is not a multiple of vector length", conditionMessage(w))) {
          warning("Unable to import file: ", file, ".\nPlease use the 'pattern' and 'skip' arguments to ensure only absorbance files are selected.", call. = FALSE)
          return(NULL)
        } else {
          stop("An unexpected error occurred: ", conditionMessage(w), call. = FALSE)}})
    }

    data <- readLines(file)
    abs <- stringr::str_extract_all(data, "-?\\d+(?:\\.\\d*)?(?:[eE][+\\-]?\\d+)?")
    abs <- lapply(abs, as.numeric)

    #if abs files was exported manually, remove header rows and extra columns in row 1
    if(length(abs[[3]]) == 0){
      #top row has two extra items
      abs[[4]] <- abs[[4]][-c(3,5)]

      #remove header items
      abs <- abs[-c(1:3)]

      #merge into df
      abs <- do.call(rbind, abs)
      abs <- abs[,c(1,8)]
    }else{
      abs <- .safe_rbind(abs)
    }

    #give column names and make into df if not skipped
    if(is.null(abs) == F){
      colnames(abs) <- c("wavelength", "absorbance")
      abs <- as.data.frame(abs)

      #thrown an error if the wavelength isn't continuous, suggesting transmittance data was added
      if(sum(diff(abs$wavelength) > 0) > 0){
        stop("wavelengths aren't continuous, please ensure transmitance data wasn't included in absorbance file:\n", file)
      }

      #create into class "abs"
      obj <- list(file = file,
                  sample = gsub(paste0("[.]", file_ext(file)), "",basename(file)),
                  n = nrow(abs),
                  data = abs,
                  dilution = NA,
                  location =dirname(file)
      )

      class(obj) <- "abs"

      attr(obj, "is_dilution_corrected") <- FALSE
      attr(obj, "is_DOC_normalized") <- FALSE
    }else{
      obj <- NULL
    }



    return(obj)
  }


#' Read absorbance data files from directory
#'
#' A flexible function that will read all absorbance files in a directory even when
#' it contains other files.
#'
#' @param input_dir path to folder containing raw absorbance files
#' @param pattern optional. optional. a character string containing a \code{\link[base]{regular expression}} to be matched to the files in input_dir.
#' only files matching the pattern will be loaded.
#' @param skip optional. a character string containing a \code{\link[base]{regular expression}} to be matched to the files in input_dir.
#' any files matching this string will be ignored. useful for ignoring EEM's data.
#' @param file_ext character. the file extension of the EEMs
#' @param recursive logical. should the function recurse into directories?
#'
#' @importFrom purrr discard reduce
#' @importFrom magrittr %>%
#' @importFrom dplyr full_join
#'
#' @returns An object of class \code{abslist} containing a list of \code{abs}.
#' For more details see \link[eemanalyzeR]{abs_read}
#' @export
#'
#' @examples
#' #load samples by using pattern to identify absorbance samples
#' abs <- abs_dir_read(system.file("extdata", package = "eemanalyzeR"), pattern = "abs|ABS")
#'
#' #load samples by using skip to exclude EEM's and other samples
#' abs <- abs_dir_read(system.file("extdata", package = "eemanalyzeR"), skip = "SEM|BEM|waterfall")
#'

#'
abs_dir_read <- function(input_dir, pattern = NULL, skip="SEM|BEM|Waterfall", file_ext="dat",
                         recursive = FALSE){
  stopifnot(dir.exists(input_dir))
  warnings_list <- list()  # Initialize an empty list to store warnings


  files <- list.files(input_dir, full.names = T, recursive = recursive)

  #only gets files with correct file extension and ones that optionally match the pattern
  ext <- paste0("[.]", file_ext, "$")
  if(is.null(pattern)){
    if(is.null(skip)){
      load_files <- files[grepl(ext, files)]
    }else{
      load_files <- files[grepl(ext, files) & !(grepl(skip, files))]
    }
  }else{
    if(is.null(skip)){
      load_files <- files[grepl(ext, files) & grepl(pattern, files)]
    }else{
      load_files <- files[grepl(ext, files) & grepl(pattern, files) & !(grepl(skip, files))]}
  }

  #read files
  abs_list <- withCallingHandlers(lapply(load_files, abs_read),
                                  warning = function(w) {
                                    warnings_list <<- c(warnings_list, conditionMessage(w))  # Add the warning message
                                    invokeRestart("muffleWarning")  # Prevent the warning from printing immediately
                                  })

    #remove nulls from trying to load eems
      abs_list <- abs_list %>% purrr::discard(is.null)

    #make into abs_list
      class(abs_list) <- "abslist"

  # Combine all collected warnings into one [probably unnecessary, I don't think anything else should generate a warning]
      if (length(warnings_list) > 0) {
        if(sum(grepl("Unable to import file: ", warnings_list))>0){
          remove_warn <- grep("Unable to import file: |.\nPlease use the 'pattern' and 'skip' arguments to ensure only absorbance files are selected.", warnings_list)
          error_files <- gsub("Unable to import file: |.\nPlease use the 'pattern' and 'skip' arguments to ensure only absorbance files are selected.", "", warnings_list)
          abs_warning <- paste0("Unable to import file(s):\n", paste(error_files, collapse="\n"),"\nPlease use the 'pattern' and 'skip' arguments to ensure only absorbance files are selected.")
        }
        combined_warning <- paste(warnings_list[-remove_warn], abs_warning, collapse = "\n")
        warning(combined_warning)
      }

    return(abs_list)
  }

#' Read metadata file from directory
#'
#' Reads a .csv or .xlsx file containing metadata related to absorbance and EEM's data.
#' Please see \link[eemanalyzeR]{metadata} for the required columns and structure.
#'
#' @param input_dir path to folder containing metadata file
#' @param name name of metadata file. optional if metadata is only xlsx or csv file in input_dir
#' if not specified function will attempt to load any xlsx or csv file in directory and return an error if there is more than one
#' @param sheet name of sheet containing metadata. only required if metadata isn't the first sheet
#' @param validate logical. should metadata structure be checked for potential issues that will cause issues during
#' further processing? highly recommended to keep as TRUE.
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr mutate across any_of
#' @importFrom utils read.csv
#'
#' @returns a data.frame containing metadata of samples
#' @export
#'
#' @examples
#' metadata <- meta_read(system.file("extdata", package = "eemanalyzeR"))
meta_read <- function(input_dir, name=NULL, sheet=NULL, validate=T){
  stopifnot(dir.exists(input_dir)) #make sure directory exists

  #get name of metadata file
    #if name isn't specified, it pulls all xlsx/csv files, assuming there will only be one
      #if name is specified, it pulls that from list of xlsx/csv files
      if(is.null(name)){
        meta_file <- list.files(input_dir, pattern="xlsx|csv", full.names = T)
      }else{
        meta_file <- grep(name, list.files(input_dir, pattern="xlsx|csv", full.names = T), value=T)
      }

  #check we have one and only one file to read in
    if(length(meta_file) > 1){
      stop("attempted to load more than one file, please use 'name' argument to specify metadata file")
    }
    if(length(meta_file) == 0){
      stop("unable to locate metadata in: ", input_dir, "\nplease ensure metadata is in folder and is a .csv or .xlsx file")
    }

  #read in metadata
    if(tools::file_ext(meta_file) == "xlsx"){
      meta <- readxl::read_excel(meta_file, sheet=sheet)
    } else{
      meta <- read.csv(meta_file)
    }

  #ensure metadata conforms to standards for futher processing
  if(validate == T){
    #check required columns are there
      req_cols <- c("data_identifier", "replicate_no", "integration_time_s","run_type", "RSU_area_1s",
                    "dilution")
      missing_cols <- setdiff(req_cols, colnames(meta))
      if(length(missing_cols) > 0) {
        stop("metadata (", basename(meta_file), ") is missing required column(s): ", paste(missing_cols, collapse = ", "))
      }

    #ensure integration time and RSU adjust area (and DOC/dilution are numeric)
      meta <- meta %>% dplyr::mutate(dplyr::across(dplyr::any_of(c("integration_time_s","RSU_area_1s", "dilution", "DOC_mg_L")), as.numeric))


    #ensure data_identifier isn't missing data
      if(any(is.na(meta$data_identifier) | is.character(meta$data_identifier) == F)){
        stop("metadata (", basename(meta_file), ") missing data identifiers for one or more samples, \nsee help(eemanalyzeR::metadata) for more info")
      }

   #make sure there aren't any duplicated samples
      unique_ID <- paste(meta$data_identifier, meta$replicate_no, meta$integration_time_s, sep="_")
      if(any(duplicated(unique_ID))){
        stop("duplicate samples found with the same data_identifier, replicate_no, and integration time:\n", paste(unique(unique_ID[duplicated(unique_ID)]), sep="\n"), "\nif they are indeed duplicates, please use 'replicate_no' to note this by using 1,2,3...")
      }

    #ensure RSU_area_1s isn't missing data
      if(any(is.na(meta$RSU_area_1s) | is.numeric(meta$RSU_area_1s) == F)){
        stop("metadata (", basename(meta_file), ") missing values for RSU adjust area, \nsee help(eemanalyzeR::metadata) for more info")
      }

    #check dilution isn't missing or 0, if it is assume it's 1 with a warning
      if(any(meta$dilution == 0 | is.na(meta$dilution))){
        meta$dilution[meta$dilution == 0 | is.na(meta$dilution)] <- 1
        warning("dilutions were missing or set to 0. assuming a value of 1 for 'dilution' which indicates no dilution")
      }

    #check replicate numbers aren't missing, if it is assume it's 1 with a warning
      if(any(is.na(meta$replicate_no))){
        meta$replicate_no[is.na(meta$replicate_no)] <- 1
        warning("replicate numbers were missing. assuming a value of 1 for 'replicate_no' which indicates no replicates")
      }

    #ensure run_type is either sampleQ or manual (might not matter anymore??)
      if(any(grepl("sampleQ|manual", meta$run_type, ignore.case = T)==F)){
        stop("'run_type' must be either 'sampleQ' or 'manual'")
      }

  }

  return(meta)
}

add_meta <- function(meta, abs=NULL, eem=NULL){
  stopifnot(class(meta) == "data.frame")

  #add metadata to absorbance data
  if(!is.null(abs)){
    names <- abs_names(abs)

    meta_order <- sapply(meta$data_identifier, grep, names) #get order of absorbance in metadata

    if(length(abs) > length(meta_order)){
      #give warning about samples not in metadata
      missing_meta <- setdiff(1:length(abs), as.numeric(meta_order))
        warning("the following absorbance data are missing from metadata:\n", paste(names[missing_meta]), sep="\n")
    }
  }

}
