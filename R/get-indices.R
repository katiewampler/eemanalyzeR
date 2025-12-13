#' Get fluorescence and absorbance indices
#'
#' Calculates commonly used indices from absorbance and excitation–emission
#' matrix (EEM) data. Also checks and flags index values for potential issues.
#'
#' @param eemlist An `eemlist` object.
#' @param abslist An `abslist` object.
#' @param index_method Either "eemanalyzeR", "eemR", "usgs", or a custom
#'   function. See **Details**.
#' @param tolerance Maximum percent deviation that the check standard can vary
#'   from the long-term values without being flagged.
#' @param return Output format: "long" or "wide".
#' @param cuvle Cuvette (path) length in cm.
#' @param qaqc_dir File path to the QAQC files generated with [create_mdl()]
#' and [create_std()]. Default is a user-specific data directory
#' [rappdirs::user_data_dir()].
#' @param arg_names Optional list of arguments passed from higher-level
#'   functions for README generation.
#'
#' @export
#' @md
#'
#' @return A list with two data frames:
#'
#' - **eem_index**: fluorescence indices
#' - **abs_index**: absorbance indices
#'
#' ## Long format
#'
#' If `return = "long"`, each data frame includes:
#'
#' - `sample_name`: sample ID
#' - `meta_name`: sample name in metadata (or `sample_name` if missing)
#' - `index`: index name
#' - `value`: index value
#' - `QAQC_flag`: any associated flags
#'
#' ## Wide format
#'
#' If `return = "wide"`, each row corresponds to a sample, and columns represent
#' indices. Flags appear:
#'
#' - *alone*, if no numeric value could be returned
#' - in the form VALUE_FLAG when a value exists
#'
#' @details
#' ## Index methods
#'
#' Three preset index sets are available:
#'
#' - **eemanalyzeR**: [eemanalyzeR_indices()]
#' - **eemR**: [eemR_indices()]
#' - **usgs**: [usgs_indices()]
#'
#' You may also pass a custom index-generating function. See the
#' [custom-indices vignette](../doc/custom-indices.html) for
#' instructions.
#'
#' ## QA/QC flags
#'
#' Index values are checked for common issues. Flags include:
#'
#' - **DATA01**: Missing data required for calculation
#' - **DATA02**: Missing required wavelengths; value may be inaccurate
#' - **DATA03**: Ratio denominator was zero
#' - **DATA04**: Spectral slope could not be calculated
#' - **DOC01**: Missing dissolved organic carbon (DOC) data
#' - **INF01**: Infinite value
#' - **MDL01**: All values below MDL
#' - **MDL02**: One or more values below MDL; use cautiously
#' - **MDL03**: Ratio index where numerator or denominator was entirely below MDL
#' - **NEG01**: Negative value
#' - **STD01**: Check standard value outside tolerance
#' - **VAL01**: Value below typical range
#' - **VAL02**: Value above typical range
#'
#' @examples
#' indices <- get_indices(
#'   example_processed_eems,
#'   example_processed_abs,
#'   qaqc_dir = system.file("extdata", package = "eemanalyzeR")
#' )
get_indices <- function(eemlist, abslist, index_method = "eemanalyzeR",
                        tolerance = 0.2, return = "long",
                        cuvle = 1, qaqc_dir = NULL, arg_names = NULL) {
  stopifnot(.is_eemlist(eemlist), .is_abslist(abslist))

  #specify qaqc dir if not specified
  if(is.null(qaqc_dir)){qaqc_dir = .qaqc_dir()}

  # check if processing has been done, not warn that indices may be unreliable
  steps <- check_processing(eemlist)
  steps <- steps[-nrow(steps), ] # remove check for DOC

  if (all(!steps$done)) {
    warning("Data has not been processed, and indices may not be accurate. \nPlease use eemanalyzeR::process_eem to process EEMs before generating indices.")
  } else if (any(!steps$done)) {
    missing <- steps[steps$done == FALSE, ]
    warning(
      "Data has not been fully processed, and indices may not be accurate. The following processing steps are missing:\n",
      paste(missing$warning, collapse = "\n"), "\n\nPlease use eemanalyzeR::process_eem to process EEMs before generating indices."
    )
  }

  # collect arguments for readme, and to put into the following functions
  if (is.null(arg_names)) {
    args <- list(
      index_method = index_method,
      return = return,
      cuvle = cuvle,
      qaqc_dir = qaqc_dir
    )

  } else {
    args <- arg_names
  }

  # if DOC normalized, make not normalized to not normalize twice for indices
  eemlist <- lapply(eemlist, function(x) {
    if (attr(x, "is_doc_normalized")) {
      x$x <- x$x * x$doc_mgL # multiply by doc to get un-normalized EEM's values
      attr(x, "is_doc_normalized") <- FALSE
    } else {
      x
    }
    return(x)
  })
  class(eemlist) <- "eemlist"

  # get function to get indices
  index_function <- get_indices_function(index_method)

  # get indices
  indices <- index_function(eemlist, abslist, cuvle = cuvle, qaqc_dir = qaqc_dir)


  # initialize QAQC flag
  indices <- lapply(indices, function(x) {
    if (inherits(x, "data.frame")) {
      x$QAQC_flag <- NA
    }
    return(x)
  })

  # flag if needed
  # helper functions to make flags
  missing_doc_flag <- function(index) {
    # if index is NA, return NA
    if (!is.data.frame(index)) {
      return(index)
    }
    doc_flag <- grepl("SUVA|SVA|DOC", index$index) & is.na(index$value) & is.na(index$QAQC_flag)
    doc_flag <- ifelse(doc_flag, "DOC01", NA)
    index$QAQC_flag <- .combine_flags(index$QAQC_flag, doc_flag)
    return(index)
  }
  negative_flag <- function(index) {
    # if index is NA, return NA
    if (!is.data.frame(index)) {
      return(index)
    }
    negative <- index$value < 0
    negative[is.na(negative)] <- FALSE
    index$value[negative] <- NA
    negative <- ifelse(negative, "NEG01", NA)
    index$QAQC_flag <- .combine_flags(index$QAQC_flag, negative)
    return(index)
  }
  move_flags <- function(index) {
    # if index is NA, return NA
    if (!is.data.frame(index)) {
      return(index)
    }

    # move flags from value column to QA/QC column and replace with NA or value
    # check if there's a flag (not numeric)
    flagged <- !grepl("^[-+]?\\d*(\\.\\d+)?([eE][-+]?\\d+)?$", index$value) & !is.na(index$value)

    if (any(flagged)) {
      # move flag values and replace with NA
      index$QAQC_flag[flagged] <- index$value[flagged]
      index$value[flagged] <- NA

      # move provisional values back to value
      prov <- grepl("^-?[0-9]{1,}\\.[0-9]{1,}\\_[A-Z]{2,}[0-9]{1,}$", index$QAQC_flag)
      index$value[prov] <- stringr::str_split_i(index$QAQC_flag[prov], "_", i = 1)
      index$QAQC_flag[prov] <- gsub("^-?[0-9]{1,}\\.[0-9]{1,}\\_", "", index$QAQC_flag[prov])
    }

    return(index)
  }
  outside_range <- function(index) {
    # if index is NA, return NA
    if (!is.data.frame(index)) {
      return(index)
    }
    ranges <- eemanalyzeR::indice_ranges[eemanalyzeR::indice_ranges$index_method == index_method, ]

    index <- plyr::join(index, ranges, by = "index")
    low <- as.numeric(index$value) < index$low_val
    low[is.na(low)] <- FALSE
    low <- ifelse(low, "VAL01", NA)
    index$QAQC_flag <- .combine_flags(index$QAQC_flag, low)

    high <- as.numeric(index$value) > index$high_val
    high[is.na(high)] <- FALSE
    high <- ifelse(high, "VAL02", NA)
    index$QAQC_flag <- .combine_flags(index$QAQC_flag, high)

    index <- index %>% dplyr::select(-any_of(c("low_val", "high_val", "sources", "index_method")))

    return(index)
  }
  infinite_flag <- function(index) {
    # if index is NA, return NA
    if (!is.data.frame(index)) {
      return(index)
    }
    infinite <- is.infinite(index$value)
    infinite[is.na(infinite)] <- FALSE
    index$value[infinite] <- NA
    infinite <- ifelse(infinite, "INF01", NA)
    index$QAQC_flag <- .combine_flags(index$QAQC_flag, infinite)
    return(index)
  }
  std_flag <- function(index, std_check) {
    if (!is.data.frame(index)) {
      return(index)
    }
    index <- index %>% dplyr::left_join(std_check, by = c("index", "meta_name"))
    index$QAQC_flag <- .combine_flags(index$QAQC_flag, index$flag)
    index <- index %>% select(-c("type", "flag"))
    return(index)
  }

  # inf values (needs to go first before moving flags)
  indices <- lapply(indices, infinite_flag)

  # move flags from value to QAQC flag col
  indices <- lapply(indices, move_flags)

  # negative values
  indices <- lapply(indices, negative_flag)

  # missing data (no DOC): DOC_01
  indices <- lapply(indices, missing_doc_flag)

  # questionable, outside normal range
  if (is.character(index_method)) {
    indices <- lapply(indices, outside_range)
  }

  # check check standards
  std_check <- check_std(eemlist, abslist,
    qaqc_dir = qaqc_dir,
    index_method = index_method, tolerance = tolerance
  )
  indices <- lapply(indices, std_flag, std_check)

  # make indices numeric
  indices <- lapply(indices, function(x) {
    # if index is NA, return NA
    if (!is.data.frame(x)) {
      return(x)
    }
    x$value <- as.numeric(x$value)
    return(x)
  })

  # change missing NA values to -9999 to indicate they're missing on purpose
  indices <- lapply(indices, function(x) {
    # if index is NA, return NA
    if (!is.data.frame(x)) {
      return(x)
    }
    x$value[is.na(x$value)] <- -9999
    return(x)
  })

  indices <- lapply(indices, function(x) {
    # if index is NA, return NA
    if (!is.data.frame(x)) {
      return(x)
    }
    x$QAQC_flag[is.na(x$QAQC_flag)] <- "N/A"
    return(x)
  })

  # return
  if (return == "wide") {
    indices <- lapply(indices, function(x) {
      # if index is NA, return NA
      if (!is.data.frame(x)) {
        return(x)
      }
      x$value_flag <- NA
      x$value_flag[x$value == -9999] <- x$QAQC_flag[x$value == -9999]
      x$value_flag[x$QAQC_flag == "N/A"] <- signif(x$value[x$QAQC_flag == "N/A"], 4)
      x$value_flag[x$value != -9999 & x$QAQC_flag != "N/A"] <- paste(signif(x$value[x$value != -9999 & x$QAQC_flag != "N/A"], 4),
        x$QAQC_flag[x$value != -9999 & x$QAQC_flag != "N/A"],
        sep = "_"
      )

      x_wide <- x %>%
        dplyr::select(-any_of(c("value", "QAQC_flag"))) %>%
        tidyr::pivot_wider(names_from = "index", values_from = "value_flag")
      return(x_wide)
    })
  }

  # write step to readme
  if (is.character(index_method)) {
    .write_readme_line("Absorbance and fluorescence indices were calculated using the 'get_indices' function", "indices", args)
  } else {
    .write_readme_line("Absorbance and fluorescence indices were calculated using the 'get_indices' function with a custom index method", "indices", args)
  }
  return(indices)
}
