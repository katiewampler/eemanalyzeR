#' Perform dilution corrections
#'
#' Adjusts the EEM's and absorbance data fluorescence to account for dilutions performed prior to measurement.
#'
#' @md
#'
#' @details
#' The function uses the dilution factor provided in the metadata to correct the EEMs and
#' absorbance values. Because of this, samples must already have metadata added using
#' [add_metadata()].
#'
#' @param x An `eemlist` or `abslist` object.
#'
#' @returns An object of the same class as `x` (`eemlist` or `abslist`) with
#'   dilution corrections applied.
#'
#' @export
#'
#' @examples
#' eemlist <- add_metadata(metadata, example_eems)
#' correct_eem <- correct_dilution(eemlist)
#'
#' abslist <- add_metadata(metadata, example_abs)
#' correct_abs <- correct_dilution(abslist)
correct_dilution <- function(x) {
  if (!any(.meta_added(x))) {
    stop("metadata must be added to data to correct samples. \nPlease add metadata using 'add_metadata' function")
  }

  dilution_factor <- get_sample_info(x, "dilution")

  # make into fraction to use normalization function
  if (any(dilution_factor > 1)) {
    dilution_factor <- 1 / dilution_factor
  }

  # don't correct any that have already been corrected
  dilution_factor[sapply(x, attr, "is_dil_corrected")] <- 1

  if (.is_eemlist(x)) {
    res <- eem_normalize(x, dilution_factor)
  }

  if (.is_abslist(x)) {
    res <- mapply(function(abs, factor) {
      abs$data[, 2] <- abs$data[, 2] / factor
      return(abs)
    }, x, dilution_factor, SIMPLIFY = F)
  }

  res <- lapply(1:length(res), function(i) {
    attr(res[[i]], "is_dil_corrected") <- TRUE
    return(res[[i]])
  })

  # write processing to readme
  if (.is_eemlist(x)) {
    .write_readme_line("EEMs data was corrected for dilutions via 'correct_dilution' function", "eem_dil_corrected", NULL)
  }
  if (.is_abslist(x)) {
    .write_readme_line("Absorbance data was corrected for dilutions via 'correct_dilution' function", "abs_dil_corrected", NULL)
  }

  class(res) <- class(x)
  return(res)
}
