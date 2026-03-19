#' Add metadata to absorbance and EEM data
#'
#' Adds metadata from a data frame to an `abslist` or `eemlist` object as additional
#' list items.
#'
#' @md
#'
#' @param meta A `data.frame` containing metadata.
#' @param x An `eemlist` or `abslist` object.
#' @param iblank_pattern a character vector of length 1 with a regular expression that matches sample names of instrument blanks. Default is "BEM$|Waterfall ?Plot ?Blank"
#' @param sblank_pattern a character vector of length 1 with a regular expression that matches sample names of sample blanks. Default is "Blank|blank|BLK"
#' @param check_pattern  a character vector of length 1 with a regular expression that matches sample names of check standards. Default is "Tea|tea"
#'
#' @note
#' If an `eemlist` contains blanks, the blanks automatically inherit metadata from their
#' corresponding sample.
#'
#' @return
#' An object of the same class as the input (`abslist` or `eemlist`), with metadata added.
#'
#' For each sample, the following fields may be added (if present in the metadata):
#'
#' - **meta_name**: identifier for the sample
#' - **dilution**: sample dilution factor
#' - **analysis_date**: date the sample was run
#' - **description**: optional description
#' - **sample_type**: optional flag (e.g., `sample` for a sample,
#'    `sblank` for an analytical blank, `check` for a check standard).
#'   Default values match Horiba Aqualog exports.
#' - **doc_mgL**: dissolved organic carbon concentration in mg/L
#' - **notes**: optional notes on sample or sampling conditions
#'
#' If `x` is an `eemlist`, two additional items are added:
#'
#' - **integration_time_s**: integration time of the sample
#' - **raman_area_1s**: Raman water peak area normalized to 1-second integration time.
#'
#' @export
#'
#' @examples
#' # Add metadata to absorbance data
#' abs_augment <- add_metadata(metadata, example_abs)
#'
#' # Add metadata to EEM data
#' eem_augment <- add_metadata(metadata, example_eems)
add_metadata <- function(meta, x,
                           iblank_pattern = "BEM$|Waterfall ?Plot ?Blank",
                           sblank_pattern = "Blank|blank|BLK",
                           check_pattern = "Tea|tea"
                         ) {

  class_type <- class(x)

  stopifnot("data.frame" %in% class(meta), class_type %in% c("abslist", "eemlist"),
            length(x) > 0)

  names <- get_sample_info(x, "sample")

  # Reorganize EEMlist or ABSlist to match metadata ----
  meta_order <- data.frame(eem_pos = 1:length(names), meta_row = NA)

  .get_row_meta <- function(name, meta) {
    name <- gsub("([\\(\\)\\-])", "\\\\\\1", name) # escape special characters
    meta$data_identifier <- gsub("([\\(\\)\\-])", "\\\\\\1", meta$data_identifier)

    row <- which(sapply(meta$data_identifier, grep, name, fixed = T) == 1)

    if (length(row) > 1) {
      row <- row[which.max(nchar(meta$data_identifier)[row])]
    }

    if (length(row) == 0) {
      row <- NA
    }
    return(row)
  }

  meta_order <- sapply(names, .get_row_meta, meta) # get order of eems in metadata

  # return error if more than one row
  if (inherits(meta_order, "list")) {
    stop("samples matched more than one row in the metadata, please correct and rerun")
  }
  names(meta_order) <- names
  # meta_order is the order of the eems where the number is the corresponding row of the metadata

  # check for missing samples either in metadata or in abs

  if (any(is.na(meta_order))) {
    # give warning about samples not in metadata
    missing_meta <- names[is.na(meta_order)]
    warning(
      "the following data are missing from metadata:\n", paste(missing_meta, collapse = "\n"),
      "\nthese sample will be removed from further processing"
    )
    x <- x[!is.na(meta_order)]
    meta_order <- na.omit(meta_order)
  }

  # Check that metadata and Aqualog files have the same number ----
  if (length(unique(meta_order)) < nrow(meta)) {
    warning(
      "the following sample is in metadata but was missing in data:\n",
      paste(meta$data_identifier[setdiff(1:nrow(meta), meta_order)], collapse = "\n"),
      "\nthese sample will be removed from further processing"
    )
    meta <- meta[-setdiff(1:nrow(meta), meta_order), ]
    meta_order <- sapply(names, .get_row_meta, meta) # get order of eems in metadata

  }

  # Deal with sample_types ----
  # Plan:
  # Old metadata version: Guess sample_type if they aren't specified

  # New metadata version: use sample_type to define samples, sample blanks, and check standards
  #                       the rest of the eems/abs not in the metadata are instrument blanks

  if (!("sample_type" %in% names(meta))) {
    # Warn the user if sample types are pattern matched?
    warning("No sample_type in Metadata. Guessing sample_types by pattern matching data_identifier")
    # Guess instrument blanks
    inst_blank_flags <- sapply(names,
      \(s) grepl(
        pattern = iblank_pattern,
        x = s,
        ignore.case = FALSE
      ),
      USE.NAMES = FALSE
    )
    # Guess checks
    check_flags <- sapply(names,
      \(s) grepl(
        pattern = check_pattern,
        x = s,
        ignore.case = FALSE
      ),
      USE.NAMES = FALSE
    )
    # Guess sample blanks
    sample_blank_flags <- sapply(names,
      \(s) grepl(
        pattern = sblank_pattern,
        x = s,
        ignore.case = FALSE
      ),
      USE.NAMES = FALSE
    )

    # Put all these together to come up with one sample type per name
    cat_flags <- paste0(
      as.integer(inst_blank_flags),
      as.integer(sample_blank_flags),
      as.integer(check_flags)
    )
    # Use these flags to assign eems
    convert_sample_flags <- function(x) {
      switch(formatC(x, width = 3, flag = "0"),
        "111" = "iblank",
        "110" = "iblank",
        "101" = "iblank",
        "100" = "iblank",
        "010" = "sblank",
        "001" = "check",
        "000" = "sample",
        stop("Sample Type Not Guessed Successfully")
      )
    }
    sample_types <- sapply(cat_flags, convert_sample_flags, USE.NAMES = FALSE)

    # Sample type in metadata for eemslist
  } else if (class_type == "eemlist") {
    # starting guess
    meta_sample_type <- meta$sample_type[meta_order]
    # Pattern match the iblank pattern
    inst_blank_flags <- sapply(names,
      \(s) grepl(
        pattern = iblank_pattern,
        x = s,
        ignore.case = FALSE
      ),
      USE.NAMES = FALSE
    )
    # Any that match the iblank pattern are iblanks
    sample_types <- ifelse(inst_blank_flags,
      "iblank",
      meta_sample_type
    )

    # Different logic for abslists
  } else if (class_type == "abslist") {
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

    # Optional
    analysis_date = if ("analysis_date" %in% colnames(meta)) meta$analysis_date[meta_order] else NA,
    description = if ("description" %in% colnames(meta)) meta$description[meta_order] else NA,
    doc_mgL = if ("DOC_mg_L" %in% colnames(meta)) meta$DOC_mg_L[meta_order] else NA,
    notes = if ("Notes" %in% colnames(meta)) meta$Notes[meta_order] else NA
  )

  # remove samples in metadata that don't have samples, do after because otherwise the meta_order doesn't match meta
  if (length(unique(meta_order)) < nrow(meta)) {
    warning(
      "the following sample is in metadata but was missing in data:\n",
      paste(meta$data_identifier[setdiff(1:nrow(meta), meta_order)], collapse = "\n"),
      "\nthese sample will be removed from further processing"
    )
    meta <- meta[-setdiff(1:nrow(meta), meta_order), ]
  }

  # loop across the metadata
  x <- lapply(1:length(meta_order), function(y) {
    obj <- x[[y]]

    # assign sample name and dilution
    obj$meta_name <- meta_data$meta_name[y]
    obj$dilution <- meta_data$dilution[y]

    if (.is_eemlist(x)) {
      obj$integration_time_s <- meta_data$integration_time_s[y]
      obj$raman_area_1s <- meta_data$raman_area_1s[y]
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

  # reorder to metadata
  x <- x[order(meta_order)]

  # ensure object returned is same type as input
  if (class_type == "abslist") {
    class(x) <- "abslist"
    stopifnot(.is_abslist(x))
  }

  if (class_type == "eemlist") {
    class(x) <- "eemlist"
    stopifnot(.is_eemlist(x))
  }


  return(x)
}
