#' Read optics data from directory
#'
#' Wrappers for \link[eemR]{eem_read} and \link[eemanalyzeR]{abs_read} to read all EEMs or absorbance
#' in a directory into R, even when it contains other files.
#'
#' @param input_dir path to folder containing raw EEMs and/or absorbance files
#' @param pattern optional. a character string containing a \code{\link[base]{regular expression}}
#' to be matched to the files in input_dir.
#' only files matching the pattern will be loaded.
#' @param skip a character string containing a \code{\link[base]{regular expression}} to be
#' matched to the files in input_dir.
#' any files matching this string will be ignored.
#' @param file_ext character. the file extension of the EEMs or absorbance
#' @param recursive logical. should the function recurse into directories?
#' @param import_function character or a user-defined function to import an EEM.
#' for more details see \link[eemR]{eem_read}

#' @name dir_read
#' @rdname dir_read
#' @importFrom eemR eem_read
#' @importFrom purrr discard
#' @importFrom magrittr %>%
#'
#' @return \code{eem_dir_read} returns an object of class eemlist,  containing a list of \code{eem}.
#' For more details see \link[eemR]{eem}
#'
#' \code{abs_dir_read} returns an object of class abslist, containing a list of \code{abs}.
#' For more details see \link[eemanalyzeR]{abs_read}
#' @export
#'
#' @examples
#' #load EEM's data
#' # Load all data from directory --------------
#' eem_list <- eem_dir_read(system.file("extdata", package = "eemanalyzeR"))
#'
#' # Load all data that matches a pattern from directory -------------
#' eem_list <- eem_dir_read(system.file("extdata", package = "eemanalyzeR"), pattern="SEM")
#'
#' #load absorbance data
#' #load samples by using pattern to identify absorbance samples -------------
#' abs <- abs_dir_read(system.file("extdata", package = "eemanalyzeR"), pattern = "abs|ABS")
#'
#' #load samples by using skip to exclude EEM's and other samples -------------
#' abs <- abs_dir_read(system.file("extdata", package = "eemanalyzeR"), skip = "SEM|BEM|waterfall")
#'
eem_dir_read <- function(input_dir,
                         pattern = NULL,
                         skip="(?i)abs",
                         file_ext="dat",
                         recursive = FALSE,
                         import_function="aqualog"){
  stopifnot(dir.exists(input_dir))

  #wrapper on eemR::read_eem to try and catch errors from absorbance data being included
  .try_eem_read <- function(file, recursive=F, import_function){
    tryCatch(
      {eem <- eemR::eem_read(file=file, recursive=recursive, import_function = import_function)
      #add additional attributes
      attr(eem[[1]], "is_doc_normalized") <- FALSE
      attr(eem[[1]], "is_dil_corrected") <- FALSE
      # Default these to false and add them later
      attr(eem[[1]], "is_blank") <- FALSE
      attr(eem[[1]], "is_check") <- FALSE

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
  # RPC refactored this because I don't like nested loops. Also wanted to check the tests
  if (is.null(pattern)) {
    pattern_choices <- rep(TRUE, length(files)) # We want NULL to select all files
  } else {
    pattern_choices <- grepl(pattern, files)
  }

  if(is.null(skip)){
    skip_choices <- rep(TRUE, length(files))
  } else{
    skip_choices <- !grepl(skip, files)
  }
  ext_choices <- tools::file_ext(files) == file_ext
  load_files <- files[which(pattern_choices & skip_choices & ext_choices)]

  #read files
  eem_list <- withCallingHandlers(
    lapply(load_files, .try_eem_read, import_function=import_function),
    warning = function(w) {
      append_warning(conditionMessage(w))  # Add the warning message
      invokeRestart("muffleWarning")  # Prevent the warning from printing immediately
      })

  # Combine all collected warnings into one [probably unnecessary, I don't think anything else should generate a warning]
  # if (length(warnings_list) > 0) {
  #   if(sum(grepl("Unable to import file: ", warnings_list))>0){
  #     remove_warn <- grep("Unable to import file: |.\nPlease use the 'pattern' and 'skip' arguments to ensure only EEM's files are selected.", warnings_list)
  #     error_files <- gsub("Unable to import file: |.\nPlease use the 'pattern' and 'skip' arguments to ensure only EEM's files are selected.", "", warnings_list)
  #     abs_warning <- paste0("Unable to import file(s):\n", paste(error_files, collapse="\n"),"\nPlease use the 'pattern' and 'skip' arguments to ensure only EEM's files are selected.")
  #   }
  #   combined_warning <- paste(warnings_list[-remove_warn], abs_warning, collapse = "\n")
  #   warning(combined_warning)
  # }

  #combine eems
  eem_list <- lapply(eem_list, `[[`, 1)
  eem_list <- eem_list %>% purrr::discard(is.null)
  class(eem_list) <- "eemlist"

  return(eem_list)
}

#' @rdname dir_read
#' @export
abs_dir_read <- function(input_dir,
                         pattern = NULL,
                         skip="SEM|BEM|Waterfall",
                         file_ext="dat",
                         recursive = FALSE) {
  stopifnot(dir.exists(input_dir))

  files <- list.files(input_dir, full.names = T, recursive = recursive)

  #only gets files with correct file extension and ones that optionally match the pattern
  # RPC refactored this because I don't like nested loops. Also wanted to check the tests
  if (is.null(pattern)) {
    pattern_choices <- rep(TRUE, length(files)) # We want NULL to select all files
  } else {
    pattern_choices <- grepl(pattern, files)
  }

  if(is.null(skip)){
    skip_choices <- rep(TRUE, length(files))
  } else{
    skip_choices <- !grepl(skip, files)
  }
  ext_choices <- tools::file_ext(files) == file_ext
  load_files <- files[which(pattern_choices & skip_choices & ext_choices)]

  #read files
  abs_list <- withCallingHandlers(lapply(load_files, abs_read),
                                  warning = function(w) {
                                    append_warning(conditionMessage(w))  # Add the warning message
                                    invokeRestart("muffleWarning")  # Prevent the warning from printing immediately
                                  })

  #remove nulls from trying to load eems
  abs_list <- abs_list %>% purrr::discard(is.null)

  #make into abs_list
  class(abs_list) <- "abslist"

  # Combine all collected warnings into one [probably unnecessary, I don't think anything else should generate a warning]
  # if (length(warnings_list) > 0) {
  #   if(sum(grepl("Unable to import file: ", warnings_list))>0){
  #     remove_warn <- grep("Unable to import file: |.\nPlease use the 'pattern' and 'skip' arguments to ensure only absorbance files are selected.", warnings_list)
  #     error_files <- gsub("Unable to import file: |.\nPlease use the 'pattern' and 'skip' arguments to ensure only absorbance files are selected.", "", warnings_list)
  #     abs_warning <- paste0("Unable to import file(s):\n", paste(error_files, collapse="\n"),"\nPlease use the 'pattern' and 'skip' arguments to ensure only absorbance files are selected.")
  #   }
  #   combined_warning <- paste(warnings_list[-remove_warn], abs_warning, collapse = "\n")
  #   warning(combined_warning)
  # }

  return(abs_list)
}



