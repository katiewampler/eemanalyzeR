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

  #ensure metadata conforms to standards for further processing
  if(validate == T){
    meta <- meta_check(meta)
  }

  return(meta)
}
