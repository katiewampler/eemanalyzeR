#' Read metadata from a file
#'
#' Reads a `.csv` or `.xlsx` file containing metadata for absorbance and
#' EEM data. See [eemanalyzeR::metadata] for required columns and structure.
#'
#' @param input Path to the metadata file or the folder containing it.
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
meta_read <- function(input,
                      sheet = NA,
                      validate_metadata = TRUE) {
  # Figure out whether the user input a directory (automatic metadata file choice)
  # or specified a file (manual file choice)

  # Do this if the input is a directory (default behavior for backwards compatibility)
  if (file_test("-d", input)) {
    # Find metadata file then assign
    meta_file <- list.files(input, pattern = "xlsx|csv", full.names = T)
    # if name isn't specified, it pulls all xlsx/csv files, assuming there will only be one

    # check we have one and only one file to read in
    if (length(meta_file) > 1) {
      stop("Multiple possible metadata files in directory. Please specify only one file")
    } else if (length(meta_file) == 0) {
      stop("Unable to locate metadata in: ", input, "\nplease ensure metadata is .csv or .xlsx file")
    }
    message("No Meta file specified, using: ", meta_file)
  }
  # Do this if the User specifies a file
  else if (file_test("-f", input)) {
    # Assign metadata file
    meta_file <- input
  }
  # Fail if neither of these options work
  else {
    stop("unable to locate metadata")
  }

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
