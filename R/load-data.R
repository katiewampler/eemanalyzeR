#' Read directory containing excitation-emission fluorescence matrices (EEM)
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
#' @param recursive logical. should the listing recurse into directories?
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

  warnings_list <- list()  # Initialize an empty list to store warnings

  #wrapper on eemR::read_eem to try and catch errors from absorbance data being included
    try_eem_read <- function(file, recursive=F, import_function){
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
    eem_list <- withCallingHandlers(lapply(load_files, try_eem_read, import_function=import_function),
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



