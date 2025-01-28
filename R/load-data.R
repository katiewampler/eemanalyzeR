#' Read excitation-emission fluorescence matrices (EEM) from directory
#'
#' A wrapper for \link[eemR]{eem_read} to read all EEMs in a directory even when
#' it contains other files.
#'
#' @param input_dir path to folder containing raw EEMs files
#' @param pattern optional. a case-sensitive character string to be matched to the files in input_dir.
#' only files matching the pattern will be loaded. to use multiple strings, use the form "str1|str2".
#' @param skip a case-insensitive character string to be matched to the files in input_dir.
#' any files matching this string will be ignored. useful for ignoring absorbance data.
#' to use multiple strings, use the form "str1|str2".
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
eem_dir_read <- function(input_dir, pattern = NULL, skip="abs", file_ext="dat",
                         recursive = FALSE, import_function="aqualog"){
  stopifnot(dir.exists(input_dir))

  warnings_list <- list()  # Initialize an empty list to store warnings

  #wrapper on eemR::read_eem to try and catch errors from absorbance data being included
    .try_eem_read <- function(file, recursive=F, import_function){
      tryCatch({eem <- eemR::eem_read(file=file, recursive=recursive, import_function = import_function)
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
          load_files <- files[grepl(ext, files) & !(grepl(skip, files, ignore.case=T))]
        }
      }else{
        if(is.null(skip)){
          load_files <- files[grepl(ext, files) & grepl(pattern, files)]
        }else{
          load_files <- files[grepl(ext, files) & grepl(pattern, files) & !(grepl(skip, files, ignore.case=T))]}
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
#' @returns a data.frame with 2 columns where the first column is the absorbance wavelength and the second column
#' is the absorbance measured.
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
      colnames(abs) <- c("wavelength", gsub(paste0("[.]", file_ext(file)), "",basename(file)))
      abs <- as.data.frame(abs)

      #thrown an error if the wavelength isn't continuous, suggesting transmittance data was added
      if(sum(diff(abs$wavelength) > 0) > 0){
        stop("wavelengths aren't continuous, please ensure transmitance data wasn't included in absorbance file:\n", file)
      }
    }

    return(abs)
  }


#' Read absorbance data files from directory
#'
#' A flexible function that will read all absorbance files in a directory even when
#' it contains other files.
#'
#' @param input_dir path to folder containing raw absorbance files
#' @param pattern optional. a case-sensitive character string to be matched to the files in input_dir.
#' only files matching the pattern will be loaded. to use multiple strings, use the form "str1|str2".
#' @param skip a case-insensitive character string to be matched to the files in input_dir.
#' any files matching this string will be ignored. useful for ignoring absorbance data.
#' to use multiple strings, use the form "str1|str2".
#' @param file_ext character. the file extension of the EEMs
#' @param recursive logical. should the function recurse into directories?
#'
#' @importFrom purrr discard reduce
#' @importFrom magrittr %>%
#' @importFrom dplyr full_join
#'
#' @returns a data.frame where the first column is the wavelengths the absorbance was measured at and each
#' subsequent column is the absorbance for a sample where the column name is the sample name.
#'
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
abs_dir_read <- function(input_dir, pattern = NULL, skip="SEM|BEM|waterfall", file_ext="dat",
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
      load_files <- files[grepl(ext, files) & !(grepl(skip, files, ignore.case=T))]
    }
  }else{
    if(is.null(skip)){
      load_files <- files[grepl(ext, files) & grepl(pattern, files)]
    }else{
      load_files <- files[grepl(ext, files) & grepl(pattern, files) & !(grepl(skip, files, ignore.case=T))]}
  }

  #read files
  abs_list <- withCallingHandlers(lapply(load_files, abs_read),
                                  warning = function(w) {
                                    warnings_list <<- c(warnings_list, conditionMessage(w))  # Add the warning message
                                    invokeRestart("muffleWarning")  # Prevent the warning from printing immediately
                                  })

    #remove nulls from trying to load eems
      abs_list <- abs_list %>% purrr::discard(is.null)

  #merge files together by wavelength
      abs <- purrr::reduce(abs_list, function(x, y) dplyr::full_join(x, y, by = "wavelength"))
      abs <- abs[order(abs$wavelength, decreasing = T),]

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

    return(abs)
  }

