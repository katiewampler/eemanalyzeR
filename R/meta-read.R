#' Read metadata from a file
#'
#' Reads a `.csv` or `.xlsx` file containing metadata for absorbance and
#' EEM data. See [eemanalyzeR::metadata] for required columns and structure.
#'
#' @param input_dir Path to the metadata file or the folder containing it.
#' @param meta_file Optional: Filename of the metadata file. If a full path is
#'   provided, it will find that specific file, otherwise it will search within input_dir
#' @param sheet Name of the sheet containing metadata (only required if the
#'   metadata is not on the first sheet of an `.xlsx` file).
#' @param validate_metadata Logical. If `TRUE`, checks the metadata for
#'   structural issues that could cause problems during processing.
#'   Recommended to keep `TRUE`.
#'
#' @return A `data.frame` containing sample metadata.
#'
#' @export
#' @md
#'
#' @examples
#' metadata <- meta_read(system.file("extdata", package = "eemanalyzeR"))

meta_read <- function(input_dir,
                      meta_file,
                      sheet = NA,
                      validate_metadata = TRUE) {
  
  # TODO: here's how we want metadata reading to go:
  # 1) If NOT given meta_file, it searches input_dir and if it finds only one csv/xlsx file, it assumes that's metadata and chugs along (with a message)
  # 2) If NOT given meta_file, it searches input_dir it finds >1 csv/xlsx file, it stops and warns the user to provide a meta_file argument that specifies which metadata to use
  # 3) If given a meta file argument, it tries to find that file and if it does, then it chugs along (no message), otherwise it errors
  meta_file <- .find_meta_file(input_dir)

  # read in metadata
  if (tools::file_ext(meta_file) == "xlsx") {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Reading .xlsx files requires the 'readxl' package. ",
        "Please install it with install.packages('readxl').",
        call. = FALSE
      )
    }
    meta <- readxl::read_excel(meta_file, sheet = ifelse(is.na(sheet),
                                                        1,
                                                        sheet))
  } else {
    meta <- read.csv(meta_file)
  }

  # ensure metadata conforms to standards for further processing
  if (validate_metadata == TRUE) {
    meta <- meta_check(meta)
  }

  return(meta)
}

# Utility function to find the meta file. Can be done inside meta_read (for backwards compatibility) or outside (as in run_eems).
.find_meta_file <- function(dir) {
    # Do this if the input is a directory (default behavior for backwards compatibility)
  if (file_test("-d", dir)) {
    # Find metadata file then assign
    meta_file <- list.files(dir, pattern = "xlsx|csv", full.names = T)
    # if name isn't specified, it pulls all xlsx/csv files, assuming there will only be one
    # check we have one and only one file to read in
    if (length(meta_file) > 1) {
      message("Multiple possible metadata files in directory. Please specify only one file")

    } else if (length(meta_file) == 0) {
      stop("Unable to locate metadata in: ", dir, "\nplease ensure metadata is .csv or .xlsx file")
    }
    message("No Meta file specified, using: ", meta_file)
  }
  # Do this if the User specifies a file
  else if (file_test("-f", dir)) {
    # Assign metadata file
    meta_file <- dir
  }
  # Fail if neither of these options work
  else {
    stop("unable to locate metadata")
  }
  return(meta_file)
}
