#' Add metadata to absorbance and EEM's data
#'
#' @param meta a \code{data.frame} of metadata
#' @param x an \code{abslist} or \code{eemlist} object
#'
#' @note if \code{eemlist} contains blanks, the blanks will get the metadata of the corresponding sample.
#'
#' @returns returns an object of the same class as the object input into the function (\code{abslist} or \code{eemlist}) with metadata added in
#' The following items are added for each sample, if available in the metadata:
#' \itemize{
#'  \item meta_name: the data identifier of the sample.
#'  \item dilution: the dilution factor for the sample.
#'  \item analysis_date: the date the sample was run.
#'  \item description: optional description of sample.
#'  \item is_blank: logical whether the sample is a blank (e.g. MilliQ water)
#'  \item is_check: logical whether the sample is a check std (e.g SRM Tea)
#'  \item doc_mgL: the concentration of dissolved organic carbon in the sample given in mg \ifelse{html}{\out{L<sup>-1</sup>}}{\eqn{L^{-1}}}
#'  \item notes: optional notes related to the sample or sample collection.
#' }
#' if \code{x} is a \code{eemlist} an additional two items are added to each sample:
#' \itemize{
#'  \item integration_time_s: the integration time for the sample.
#'  \item raman_area_1s: the area under the raman water peak with a 1 second integration time.
#' }
#' @export
#'
#' @examples
#'
#' #add metadata to absorbance data
#' abs_augment <- add_metadata(metadata, example_absorbance)
#'
#' #add metadata to EEM's data
#' eem_augment <- add_metadata(metadata, example_eems)

add_metadata <- function(meta, x){
  stopifnot("data.frame" %in% class(meta), class(x) %in% c("abslist", "eemlist"))

  class_type <- class(x)
  names <- get_sample_info(x, "sample")

  # Reorganize EEMlist or ABSlist to match metadata ----
  meta_order <- data.frame(eem_pos = 1:length(names), meta_row=NA)

  .get_row_meta <- function(name, meta){
    name <- gsub("([\\(\\)\\-])", "\\\\\\1", name) #escape special characters
    row <- which(sapply(gsub("([\\(\\)\\-])", "\\\\\\1", meta$data_identifier), grep, name, fixed=T) == 1)
    if(length(row) == 0){row <- NA}
    return(row)
  }

  meta_order <- sapply(names, .get_row_meta, meta) #get order of eems in metadata
  names(meta_order) <- names
  #meta_order is the order of the eems where the number is the corresponding row of the metadata

  # Check for missing samples ----
  if(any(is.na(meta_order))){
    #give warning about samples not in metadata
    missing_meta <- names[is.na(meta_order)]
    warning("the following data are missing from metadata:\n", paste(missing_meta, collapse="\n"),
            "\nthese sample will be removed from further processing")
    # Make this a utility function that drops abs or eems from the list
    x <- x[!is.na(meta_order)]
    meta_order <- na.omit(meta_order)
  }

  # Check that metadata and Aqualog files have the same number ----
  if(length(unique(meta_order)) < nrow(meta)){
    warning("the following sample is in metadata but was missing in data:\n",
            paste(meta$data_identifier[setdiff(1:nrow(meta), meta_order)], collapse="\n"),
            "\nthese sample will be removed from further processing")
    meta <- meta[-setdiff(1:nrow(meta), meta_order),]
  }

  # Guess is_blank and is_check if they aren't specified
  # Also make sure all sample names with BEM are considered "blanks"
  # TODO - check with Katie that this is what we want to do
  blank_pattern <- "BEM"
  blank_flags <- sapply(get_sample_info(x, "sample"),
                        \(s) grepl(
                          pattern = blank_pattern,
                          x = s,
                          ignore.case = TRUE
                        ))

  check_pattern <- "tea"
  check_flags <- sapply(get_sample_info(x, "sample"),
                        \(s) grepl(
                          pattern = check_pattern,
                          x = s,
                          ignore.case = TRUE
                        ))


  # Add metadata info to object ----
  # Get data from metadata, keeping as numeric/character
  meta_data <- list(
    # Required
    meta_name = meta$data_identifier[meta_order],
    dilution = meta$dilution[meta_order],
    integration_time_s = meta$integration_time_s[meta_order],
    raman_area_1s = meta$RSU_area_1s[meta_order],
    is_blank = meta$is_blank[meta_order],
    is_check = meta$is_check[meta_order],

    #Optional
    analysis_date = if("analysis_date" %in% colnames(meta)) meta$analysis_date[meta_order] else NA,
    description = if("description" %in% colnames(meta)) meta$description[meta_order] else NA,
    doc_mgL = if("DOC_mg_L" %in% colnames(meta)) meta$DOC_mg_L[meta_order] else NA,
    notes = if("Notes" %in% colnames(meta)) meta$Notes[meta_order] else NA
  )

  # Loop across the metadata ----
  x <- lapply(1:length(meta_order), function(y) {
    obj <- x[[y]]

    # assign sample name and dilution
    obj$meta_name <- meta_data$meta_name[y]
    obj$dilution <- meta_data$dilution[y]

    if(.is_eemlist(x)){
      obj$integration_time_s = meta_data$integration_time_s[y]
      obj$raman_area_1s = meta_data$raman_area_1s[y]
    }

    # assign values if they are in metadata
    obj$analysis_date <- meta_data$analysis_date[y]
    obj$description <- meta_data$description[y]
    obj$doc_mgL <- meta_data$doc_mgL[y]
    obj$notes <- meta_data$notes[y]

    # Assign attributes in metadata
    attr(obj, "is_blank") <- meta_data$is_blank[y] || blank_flags[y]
    attr(obj, "is_check") <- meta_data$is_check[y]


    return(obj)
  })

  #reorder to metadata
  x <- x[order(meta_order)]

  # ensure object returned is same type as input
  if(class_type == "abslist"){
    class(x) <- "abslist"
    stopifnot(.is_abslist(x))
  }

  if(class_type == "eemlist"){
    class(x) <- "eemlist"
    stopifnot(.is_eemlist(x))
  }


  return(x)
}

