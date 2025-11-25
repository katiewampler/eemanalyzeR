#' Read absorbance and fluorescence data from directory
#'
#' Wrappers for [eem_read()] and [abs_read()] to read all EEMs or absorbance
#' files in a directory into R, even when the directory contains other files.
#'
#' @param input_dir Path to folder containing raw EEMs and/or absorbance files.
#' @param pattern Optional. A character string containing a [base::regular expression()]
#'   to match files in `input_dir`. Only files matching the pattern will be loaded.
#' @param skip Optional. A character string containing a [base::regular expression()]
#'   to match files in `input_dir` that should be ignored.
#' @param file_ext The file extension of the EEMs or absorbance files.
#' @param recursive Logical. Should the function recurse into directories?
#' @param import_function Character or a user-defined function to import an EEM.
#'   For more details, see [`vignette("custom-indices")`](../doc/custom-indices.html).
#'
#' @name dir_read
#'
#' @return
#' - `eem_dir_read()` returns an object of class `eemlist`, containing a list of `eem`. For more details see [eemR::eem_read()].
#' - `abs_dir_read()` returns an object of class `abslist`, containing a list of `abs`. For more details see [abs_read()].
#'
#' @export
#' @md
#'
#' @examples
#' # Load all EEMs from directory
#' eem_list <- eem_dir_read(system.file("extdata", package = "eemanalyzeR"))
#'
#' # Load all EEMs matching a pattern from directory
#' eem_list <- eem_dir_read(system.file("extdata", package = "eemanalyzeR"), pattern = "SEM")
#'
#' # Load absorbance samples using a pattern
#' abs <- abs_dir_read(system.file("extdata", package = "eemanalyzeR"), pattern = "abs|ABS")
#'
#' # Load absorbance samples while skipping EEMs and other files
#' abs <- abs_dir_read(system.file("extdata", package = "eemanalyzeR"), skip = "SEM|BEM|waterfall")
eem_dir_read <- function(input_dir,
                         pattern = NULL,
                         skip = "(?i)abs",
                         file_ext = "dat",
                         recursive = FALSE,
                         import_function = "aqualog") {
  stopifnot(dir.exists(input_dir))

  # TODO: change to package environment
  # remove readme if it exists because new dataset
  if (exists("readme")) {
    rm("readme", envir = .GlobalEnv)
    message("NOTE: removed previous 'readme' file")
  }


  warnings_list <- list() # Initialize an empty list to store warnings

  # wrapper on eemR::eem_read to try and catch errors from absorbance data being included
  # It's not really an error in our package if absorbance data is included in the directory,
  # since we expect it all in one aqualog output directory. So maybe I can modify this function
  # to not throw warnings for Absorbance data in the dir
  .try_eem_read <- function(file, recursive = F, import_function) {
    tryCatch(
      {
        eem <- eemR::eem_read(file = file, recursive = recursive, import_function = import_function)
        # add additional attributes
        attr(eem[[1]], "is_doc_normalized") <- FALSE
        attr(eem[[1]], "is_dil_corrected") <- FALSE
        # Default these to none and add them later
        attr(eem[[1]], "sample_type") <- "none"

        return(eem)
      },
      error = function(e) {
        # Check if it's a specific error
        if (grepl("argument of length 0", conditionMessage(e))) {
          warning("Unable to import file: ", file, ".\nPlease use the 'pattern' and 'skip' arguments to ensure only EEM's files are selected.", call. = FALSE)
          return(NULL)
        } else {
          stop("An unexpected error occurred: ", conditionMessage(e), call. = FALSE)
        }
      }
    )
  }

  # get files to read in
  files <- list.files(input_dir, full.names = T, recursive = recursive)

  # only gets files with correct file extension and ones that optionally match the pattern
  # RPC refactored this because I don't like nested loops. Also wanted to check the tests
  if (is.null(pattern)) {
    pattern_choices <- rep(TRUE, length(files)) # We want NULL to select all files
  } else {
    pattern_choices <- grepl(pattern, files, ignore.case = FALSE)
  }

  if (is.null(skip)) {
    skip_choices <- rep(TRUE, length(files))
  } else {
    skip_choices <- !grepl(skip, files, ignore.case = FALSE)
  }
  ext_choices <- tools::file_ext(files) == file_ext
  load_files <- files[which(pattern_choices & skip_choices & ext_choices)]

  # read files
  eem_list <- withCallingHandlers(
    pbapply::pblapply(load_files, .try_eem_read, import_function = import_function),
    warning = function(w) {
      conditionMessage(w)
      # append_warning(conditionMessage(w))  # Add the warning message
      # invokeRestart("muffleWarning")  # Prevent the warning from printing immediately
    }
  )

  # combine eems
  eem_list <- lapply(eem_list, `[[`, 1)
  eem_list <- eem_list %>% purrr::discard(is.null)
  class(eem_list) <- "eemlist"

  return(eem_list)
}

#' @name dir_read
#' @export
abs_dir_read <- function(input_dir,
                         pattern = NULL,
                         skip = "SEM|BEM|Waterfall",
                         file_ext = "dat",
                         recursive = FALSE) {
  # TODO: change to package environment
  # remove readme if it exists because new dataset
  if (exists("readme")) {
    rm("readme", envir = .GlobalEnv)
    message("NOTE: removed previous 'readme' file")
  }

  stopifnot(dir.exists(input_dir))

  files <- list.files(input_dir, full.names = T, recursive = recursive)

  # only gets files with correct file extension and ones that optionally match the pattern
  # RPC refactored this because I don't like nested loops. Also wanted to check the tests
  if (is.null(pattern)) {
    pattern_choices <- rep(TRUE, length(files)) # We want NULL to select all files
  } else {
    pattern_choices <- grepl(pattern, files, ignore.case = FALSE)
  }

  if (is.null(skip)) {
    skip_choices <- rep(TRUE, length(files))
  } else {
    skip_choices <- !grepl(skip, files, ignore.case = FALSE)
  }
  ext_choices <- tools::file_ext(files) == file_ext
  load_files <- files[which(pattern_choices & skip_choices & ext_choices)]

  # read files
  abs_list <- withCallingHandlers(pbapply::pblapply(load_files, abs_read),
    warning = function(w) {
      warning(w)
      # append_warning(conditionMessage(w))  # Add the warning message
      # invokeRestart("muffleWarning")  # Prevent the warning from printing immediately
    }
  )

  # remove nulls from trying to load eems
  abs_list <- abs_list %>% purrr::discard(is.null)

  # make into abs_list
  class(abs_list) <- "abslist"

  return(abs_list)
}
