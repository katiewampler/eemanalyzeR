#' Subset `eemlist` or `abslist` based on components
#'
#' Helper function to select or remove samples from an `eemlist` or `abslist` using components
#' beyond just the sample name. Builds on [eemR::eem_extract()].
#'
#' @seealso [get_sample_info()]
#'
#' @param x An `eemlist` or `abslist` object.
#' @param sample A vector of names or other component values to select/exclude from `x`.
#' @param info Name of the component within the `eem` or `abs` to extract. See
#'   [eemR::eem] for base `eem` components and [abs_read()]
#'   for base `abs` components. Extended components may be added with [add_metadata()].
#' @param keep Logical. If `TRUE`, returns the specified samples; if `FALSE`, removes them. Default is `FALSE`.
#' @param ignore_case Logical. Should case be ignored when matching? Default is `FALSE`.
#' @param verbose Logical. If `TRUE`, prints removed or extracted samples to the console.
#'
#' @return An object of class `eemlist` or `abslist` with samples selected or removed based on `sample`.
#'
#' @export
#' @md
#'
#' @examples
#' # Subset by sample name
#' names <- get_sample_info(example_eems, "sample")
#' eem_subset <- subset_samples(example_eems, "sample", names[1]) # removes by default
#' eem_subset <- subset_samples(example_eems, "sample", names[1], keep = TRUE) # keeps instead
#'
#' # Subset by metadata name
#' eemlist <- add_metadata(metadata, example_eems)
#' names <- get_sample_info(eemlist, "meta_name")
#' eem_subset <- subset_samples(eemlist, "meta_name", names[1]) # removes by default
subset_samples <- function(x, info, sample, keep = FALSE, ignore_case = FALSE,
                           verbose = TRUE) {
  stopifnot(inherits(x, "eemlist") | inherits(x, "abslist"), info %in% unlist(lapply(x, names)))
  values <- get_sample_info(x, info)
  sample <- gsub("([\\(\\)\\-])", "\\\\\\1", sample) # escape special characters
  to_remove <- grepl(paste(sample, collapse = "|"), values,
    ignore.case = ignore_case
  )
  x[xor(to_remove, keep)] <- NULL
  if (verbose) {
    if (all(to_remove == FALSE)) {
      message("Nothing to remove.")
    } else {
      message(
        ifelse(keep, "Extracted sample(s):\n", "Removed sample(s):\n"),
        paste(values[to_remove], collapse = "\n"), "\n"
      )
    }
  }
  return(x)
}
