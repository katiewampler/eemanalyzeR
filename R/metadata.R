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

#' Run tests to validate metadata
#'
#' @param meta a data.frame with metadata
#' @importFrom anytime anytime
#'
#' @returns a \code{data.frame} (meta), with errors fixed
#'
#' @examples
#' metadata <- meta_read(system.file("extdata", package = "eemanalyzeR"))
#' metadata <- eemanalyzeR:::meta_check(metadata)

meta_check <- function(meta){
  #check required columns are there
  req_cols <- c("data_identifier", "replicate_no", "integration_time_s","run_type", "RSU_area_1s",
                "dilution")
  missing_cols <- setdiff(req_cols, colnames(meta))
  if(length(missing_cols) > 0) {
    stop("metadata is missing required column(s): ", paste(missing_cols, collapse = ", "))
  }

  #ensure integration time and RSU adjust area (and DOC/dilution are numeric)
  meta <- meta %>% dplyr::mutate(dplyr::across(dplyr::any_of(c("integration_time_s","RSU_area_1s", "dilution", "DOC_mg_L")), as.numeric))

  #ensure datas are numeric
  meta <- meta %>% dplyr::mutate(dplyr::across(dplyr::any_of(c("analysis_date", "collect_date")), anytime::anytime))

  #ensure data_identifier isn't missing data
  if(any(is.na(meta$data_identifier) | is.character(meta$data_identifier) == F)){
    stop("metadata missing data identifiers for one or more samples, \nsee help(eemanalyzeR::metadata) for more info")
  }

  #make sure there aren't any duplicated samples
  unique_ID <- paste(meta$data_identifier, meta$replicate_no, meta$integration_time_s, sep="_")
  if(any(duplicated(unique_ID))){
    stop("duplicate samples found with the same data_identifier, replicate_no, and integration time:\n", paste(unique(unique_ID[duplicated(unique_ID)]), sep="\n"), "\nif they are indeed duplicates, please use 'replicate_no' to note this by using 1,2,3...")
  }

  #ensure RSU_area_1s isn't missing data
  if(any(is.na(meta$RSU_area_1s) | is.numeric(meta$RSU_area_1s) == F)){
    stop("metadata missing values for RSU adjust area, \nsee help(eemanalyzeR::metadata) for more info")
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

  return(meta)
}

#' Add metadata to absorbance and EEM's data
#'
#' @param meta a \code{data.frame} of metadata
#' @param abslist an \code{abslist} object containing the corresponding absorbance data
#' @param eemlist an \code{eemlist} object containing the corresponding EEM's data
#'
#' @returns returns an object of the same class as the object input into the function (\code{abslist} or \code{eemlist}) with metadata added in
#' The following items are added for each sample, if available in the metadata:
#' \itemize{
#'  \item dilution: the dilution factor for the sample.
#'  \item analysis_date: the date the sample was run.
#'  \item description: optional description of sample.
#'  \item doc_mgL: the concentration of dissolved organic carbon in the sample given in mg \ifelse{html}{\out{L<sup>-1</sup>}}{\eqn{L^{-1}}}
#'  \item notes: optional notes related to the sample or sample collection.
#' }
#'
#' @rdname add_meta
#' @name add_meta
#' @export
#'
#' @examples
#'
#' #add metadata to absorbance data
#' abs_augment <- abs_add_meta(metadata, example_absorbance)
#'
#' #add metadata to EEM's data
#' eem_augment <- eem_add_meta(metadata, example_samples)

abs_add_meta <- function(meta, abslist){
  stopifnot("data.frame" %in% class(meta), class(abslist) == "abslist")

  names <- abs_names(abslist)

  meta_order <- sapply(meta$data_identifier, grep, names) #get order of absorbance in metadata
  #meta_order is in the order of the metadata, where the number indicates the position of that sample in the abslist

  #check for missing samples either in metadata or in abs
  if(length(abslist) > length(meta_order)){
    #give warning about samples not in metadata
    missing_meta <- setdiff(1:length(abslist), as.numeric(meta_order))
    warning("the following absorbance data are missing from metadata:\n", paste(names[missing_meta], collapse="\n"), "\nthese sample will be removed from further processing")
    abslist <- abslist[-missing_meta]
  }

  if(any(is.na(as.numeric(meta_order)))){
    warning("the following sample is in metadata but was missing in absorbance data:\n",
            paste(names(meta_order)[is.na(as.numeric(meta_order))], collapse="\n"), "\nthese sample will be removed from further processing")
    meta <- meta[!is.na(as.numeric(meta_order)),]
    meta_order <- unlist(meta_order[!is.na(as.numeric(meta_order))])
  }

  #add metadata info to abs object
  #get data from metadata, keeping as numeric/character
  meta_data <- list(
    sample = meta$data_identifier,
    dilution = meta$dilution,
    analysis_date = if("analysis_date" %in% colnames(meta)) meta$analysis_date else NULL,
    description = if("description" %in% colnames(meta)) meta$description else NULL,
    doc_mgL = if("DOC_mg_L" %in% colnames(meta)) meta$DOC_mg_L else NULL,
    notes = if("Notes" %in% colnames(meta)) meta$Notes else NULL
  )

  # loop across the metadata
  abslist <- lapply(1:length(meta_order), function(x) {
    obj <- abslist[[meta_order[x]]]

    # assign sample name and dilution
    obj$sample <- meta_data$sample[x]
    obj$dilution <- meta_data$dilution[x]

    # assign values if they are in metadata
    if (!is.null(meta_data$analysis_date)) obj$analysis_date <- meta_data$analysis_date[x]
    if (!is.null(meta_data$description)) obj$description <- meta_data$description[x]
    if (!is.null(meta_data$doc_mgL)) obj$doc_mgL <- meta_data$doc_mgL[x]
    if (!is.null(meta_data$notes)) obj$notes <- meta_data$notes[x]

    return(obj)
  })

  # ensure object returned is abslist
  class(abslist) <- "abslist"

  return(abslist)}

#' @rdname add_meta
#' @export
#'

eem_add_meta <- function(meta, eemlist){
  stopifnot("data.frame" %in% class(meta), class(eemlist) == "eemlist")

  names <- eemR::eem_names(eemlist)

  meta_order <- sapply(meta$data_identifier, grep, names) #get order of absorbance in metadata
  #meta_order is in the order of the metadata, where the number indicates the position of that sample in the abslist

  #check for missing samples either in metadata or in abs
  if(length(eemlist) > length(meta_order)){
    #give warning about samples not in metadata
    missing_meta <- setdiff(1:length(eemlist), as.numeric(meta_order))
    warning("the following EEM's data are missing from metadata:\n", paste(names[missing_meta], collapse="\n"), "\nthese sample will be removed from further processing")
    eemlist <- eemlist[-missing_meta]
  }

  if(any(is.na(as.numeric(meta_order)))){
    warning("the following sample is in metadata but was missing in EEM's data:\n",
            paste(names(meta_order)[is.na(as.numeric(meta_order))], collapse="\n"), "\nthese sample will be removed from further processing")
    meta <- meta[!is.na(as.numeric(meta_order)),]
    meta_order <- unlist(meta_order[!is.na(as.numeric(meta_order))])
  }

  #add metadata info to abs object
  #get data from metadata, keeping as numeric/character
  meta_data <- list(
    sample = meta$data_identifier,
    dilution = meta$dilution,
    analysis_date = if("analysis_date" %in% colnames(meta)) meta$analysis_date else NULL,
    description = if("description" %in% colnames(meta)) meta$description else NULL,
    doc_mgL = if("DOC_mg_L" %in% colnames(meta)) meta$DOC_mg_L else NULL,
    notes = if("Notes" %in% colnames(meta)) meta$Notes else NULL
  )

  # loop across the metadata
  eemlist <- lapply(1:length(meta_order), function(x) {
    obj <- eemlist[[meta_order[x]]]

    # assign sample name and dilution
    obj$sample <- meta_data$sample[x]
    obj$dilution <- meta_data$dilution[x]

    # assign values if they are in metadata
    if (!is.null(meta_data$analysis_date)) obj$analysis_date <- meta_data$analysis_date[x]
    if (!is.null(meta_data$description)) obj$description <- meta_data$description[x]
    if (!is.null(meta_data$doc_mgL)) obj$doc_mgL <- meta_data$doc_mgL[x]
    if (!is.null(meta_data$notes)) obj$notes <- meta_data$notes[x]

    return(obj)
  })

  # ensure object returned is abslist
  class(eemlist) <- "eemlist"

  return(eemlist)}

