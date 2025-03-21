#' Read metadata file from directory
#'
#' Reads a .csv or .xlsx file containing metadata related to absorbance and EEM's data.
#' Please see \link[eemanalyzeR]{metadata} for the required columns and structure.
#'
#' @param input path to folder containing metadata file, or name of metadata file
#' @param sheet name of sheet containing metadata. only required if metadata isn't the first sheet of xlsx file
#' @param validate logical. should metadata structure be checked for potential issues that will cause issues during
#' further processing? highly recommended to keep as TRUE.
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr mutate across any_of
#' @importFrom utils read.csv
#' @importFrom utils file_test
#'
#' @returns a data.frame containing metadata of samples
#' @export
#'
#' @examples
#' metadata <- meta_read(system.file("extdata", package = "eemanalyzeR"))
meta_read <- function(input,
                      sheet=NULL,
                      validate=T){

  # Figure out whether the user input a directory (automatic metadata file choice)
  # or specified a file (manual file choice)

  # Do this if the input is a directory (default behavior for backwards compatibility)
  if (file_test("-d", input)) {
    # Find metadata file then assign
    meta_file <- list.files(input, pattern="xlsx|csv", full.names = T)
    #if name isn't specified, it pulls all xlsx/csv files, assuming there will only be one

    # check we have one and only one file to read in
    if(length(meta_file) > 1){
      stop("Multiple possible metadata files in directory. Please specify only one file")
    } else if(length(meta_file) == 0){
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


  #read in metadata
  if(tools::file_ext(meta_file) == "xlsx"){
    meta <- readxl::read_excel(meta_file, sheet=sheet)
  } else{
    meta <- read.csv(meta_file)
  }

  #ensure metadata conforms to standards for further processing
  if(validate == T){
    meta <- meta_check(meta)
  }

  return(meta)
}
