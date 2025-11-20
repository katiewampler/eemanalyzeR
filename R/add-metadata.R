#' Add metadata to absorbance and EEM's data
#'
#' @param meta a \code{data.frame} of metadata
#' @param x an \code{abslist} or \code{eemlist} object
#' @param sample_type_regex a named list of regex strings to find instrument blanks, sample blanks, check standards, and samples using pattern matching.
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
#'  \item sample_type: optional Flag for whether the data identifier is linked to a real sample (sample), sample blank (sblank), or check standard (check). Default values are for data exports for the Horiba Aqualog with Autosampler
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
#' abs_augment <- add_metadata(metadata, example_abs)
#'
#' #add metadata to EEM's data
#' eem_augment <- add_metadata(metadata, example_eems)

add_metadata <- function(meta, x,
                         sample_type_regex = list(iblank_pattern = "BEM$",
                                                  sblank_pattern = "Blank|blank",
                                                  check_pattern = "Tea|tea")) {

  class_type <- class(x)

  stopifnot("data.frame" %in% class(meta), class_type %in% c("abslist", "eemlist"))

  names <- get_sample_info(x, "sample")

  # Reorganize EEMlist or ABSlist to match metadata ----
  meta_order <- data.frame(eem_pos = 1:length(names), meta_row=NA)

  .get_row_meta <- function(name, meta){
    name <- gsub("([\\(\\)\\-])", "\\\\\\1", name) #escape special characters
    meta$data_identifier <- gsub("([\\(\\)\\-])", "\\\\\\1", meta$data_identifier)

    row <- which(sapply(meta$data_identifier, grep, name, fixed=T) == 1)

    if(length(row) > 1){
      row <- row[which.max(nchar(meta$data_identifier)[row])]
    }

    if(length(row) == 0){row <- NA}
    return(row)
  }

  meta_order <- sapply(names, .get_row_meta, meta) #get order of eems in metadata

  #return error if more than one row
  if(inherits(meta_order, "list")){
    stop("samples matched more than one row in the metadata, please correct and rerun")
  }
  names(meta_order) <- names
  #meta_order is the order of the eems where the number is the corresponding row of the metadata

  #check for missing samples either in metadata or in abs

  if(any(is.na(meta_order))){
    #give warning about samples not in metadata
    missing_meta <- names[is.na(meta_order)]
    warning("the following data are missing from metadata:\n", paste(missing_meta, collapse="\n"),
            "\nthese sample will be removed from further processing")
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

  # Deal with sample_types ----
  # Plan:
  # Old metadata version: Guess sample_type if they aren't specified

  # New metadata version: use sample_type to define samples, sample blanks, and check standards
  #                       the rest of the eems/abs not in the metadata are instrument blanks
  
  # TODO manual sample pattern matching w/ metadata

  if (!("sample_type" %in% names(meta))) {
    # Warn the user if sample types are pattern matched?
    warning("Guessing sample_types by pattern matching data_identifier")
    # Guess instrument blanks
    inst_blank_flags <- sapply(names,
                          \(s) grepl(
                            pattern = sample_type_regex$iblank_pattern,
                            x = s,
                            ignore.case = FALSE
                          ),
                          USE.NAMES = FALSE)
    # Guess checks
    check_flags <- sapply(names,
                          \(s) grepl(
                            pattern = sample_type_regex$check_pattern,
                            x = s,
                            ignore.case = FALSE
                          ),
                          USE.NAMES = FALSE)
    # Guess sample blanks
    sample_blank_flags <- sapply(names,
                                 \(s) grepl(
                                   pattern = sample_type_regex$sblank_pattern,
                                   x = s,
                                   ignore.case = FALSE
                                 ),
                                 USE.NAMES = FALSE)
    # Put all these together to come up with one sample type per name
    cat_flags <- paste0(
      as.integer(inst_blank_flags),
      as.integer(sample_blank_flags),
      as.integer(check_flags))
    # Use these flags to assign eems
    convert_sample_flags <- function(x) {switch(
      formatC(x, width = 3, flag = "0"),
      "110" = "iblank",
      "101" = "iblank",
      "100" = "iblank",
      "010" = "sblank",
      "001" = "check",
      "000" = "sample",
      stop("Sample Type Not Guessed Successfully"))}
    sample_types <- sapply(cat_flags, convert_sample_flags, USE.NAMES = FALSE)

  # Sample type in metadata for eemslist
  } else if(class_type == "eemlist") {
    # starting guess
    meta_sample_type <- meta$sample_type[meta_order]
    # Pattern match the iblank pattern
    inst_blank_flags <- sapply(names,
                               \(s) grepl(
                                 pattern = sample_type_regex$iblank_pattern,
                                 x = s,
                                 ignore.case = FALSE
                               ),
                               USE.NAMES = FALSE)
    # Any that match the iblank pattern are iblanks
    sample_types <- ifelse(inst_blank_flags,
                           "iblank",
                           meta_sample_type)

  # Different logic for abslists
  } else if(class_type == "abslist") {
    sample_types <- meta$sample_type
  }

  # Add metadata info to object ----
  # Get data from metadata, keeping as numeric/character
  meta_data <- list(
    # Required
    meta_name = meta$data_identifier[meta_order],
    dilution = meta$dilution[meta_order],
    integration_time_s = meta$integration_time_s[meta_order],
    raman_area_1s = meta$RSU_area_1s[meta_order],
    sample_type = meta$sample_types, # No meta_order here to account for instrument blanks repeating

    #Optional
    analysis_date = if("analysis_date" %in% colnames(meta)) meta$analysis_date[meta_order] else NA,
    description = if("description" %in% colnames(meta)) meta$description[meta_order] else NA,
    doc_mgL = if("DOC_mg_L" %in% colnames(meta)) meta$DOC_mg_L[meta_order] else NA,
    notes = if("Notes" %in% colnames(meta)) meta$Notes[meta_order] else NA
  )

  #remove samples in metadata that don't have samples, do after because otherwise the meta_order doesn't match meta
    if(length(unique(meta_order)) < nrow(meta)){
      warning("the following sample is in metadata but was missing in data:\n",
              paste(meta$data_identifier[setdiff(1:nrow(meta), meta_order)], collapse="\n"),
              "\nthese sample will be removed from further processing")
      meta <- meta[-setdiff(1:nrow(meta), meta_order),]
    }

  # loop across the metadata
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

    # Assign sample_type attribute
    attr(obj, "sample_type") <- sample_types[y]

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


  return(x)}


