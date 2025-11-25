#' Convert between an `eem` object and a `data.frame`
#'
#' Sometimes it is easier to subset an EEM matrix if it is in a long format.
#' This function will take an EEM matrix and turn it into a `data.frame` or,
#' if it is a `data.frame`, convert it to an `eem` object.
#'
#' @param file Optional. File path to the EEM data including the data file name;
#'   only used if `eem` is a `data.frame`.
#' @param sample Optional. The name of the EEM sample; only used if `eem` is a `data.frame`.
#' @param location Optional. The path to the directory where the file is located; only used if `eem` is a `data.frame`.
#' @param eem An object of class `eem` or a `data.frame` with three columns: `ex`, `em`, and `fluor`.
#'
#' @details
#' If you're loading samples that have been partially processed, it's recommended to set the correct processing
#' attributes to `TRUE` to indicate these steps have already been performed. Use `attributes(eem)` to see attributes,
#' and `attr(eem, "attribute_name") <- TRUE` to set them.
#'
#' @md
#' @returns Converts between an object of class `eem` and a `data.frame` with three columns:
#'   - `ex`: the excitation wavelengths
#'   - `em`: the emission wavelengths
#'   - `fluor`: the fluorescence values
#' @export
#' @examples
#' # Convert an EEM to a long data.frame
#' flat_eem <- eem_transform(example_eems[[1]])
#'
#' # Convert back to an EEM object
#' eem_obj <- eem_transform(flat_eem)
eem_transform <- function(eem, file = NULL, sample = NULL, location = NULL) {
  stopifnot(.is_eem(eem) | is.data.frame(eem))

  if (.is_eem(eem)) {
    df <- data.frame(
      ex = rep(eem$ex, each = length(eem$em)),
      em = rep(eem$em, length(eem$ex)),
      fluor = as.vector(eem$x)
    )

    return(df)
  }

  if (is.data.frame(eem)) {
    eem_obj <- list(
      file = ifelse(is.null(file), NA, file),
      sample = ifelse(is.null(sample), NA, sample),
      x = matrix(eem$fluor, ncol = length(unique(eem$ex)), nrow = length(unique(eem$em))),
      ex = unique(eem$ex),
      em = unique(eem$em),
      location = ifelse(is.null(location), NA, location)
    )
    class(eem_obj) <- "eem"

    # add attributes
    attributes(eem_obj) <- append(attributes(eem_obj), list(
      "is_blank_corrected" = FALSE,
      "is_scatter_corrected" = FALSE,
      "is_ife_corrected" = FALSE,
      "is_raman_normalized" = FALSE,
      "is_doc_normalized" = FALSE,
      "is_dil_corrected" = FALSE,
      "sample_type" = "none"
    ))

    return(eem_obj)
  }
}
