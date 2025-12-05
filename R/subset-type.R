#' Extract samples by type from an `eemlist` or `abslist`
#'
#' Selects samples based on the `sample_type` attribute added via [add_metadata()].
#' Types include instrument blanks, analytical blanks, check standards, and regular samples.
#'
#' @param x An `eemlist` or `abslist` object.
#' @param type A character or vector specifying sample type(s) to extract. Options include:
#'   - **iblank**: instrument blank
#'   - **sblank**: analytical blank
#'   - **check**: check standard
#'   - **sample**: regular samples
#' @param negate Logical. If `TRUE`, returns all samples **except** those of
#'  the specified type(s). Default is `FALSE`.
#'
#' @return An object of the same class as `x` containing the selected (or excluded) samples.
#'
#' @export
#' @md
#'
#' @examples
#' abs <- add_metadata(metadata, example_abs)
#' eem <- add_metadata(metadata, example_eems)
#'
#' # No instrument blanks exist
#' tea <- subset_type(abs, "iblank")
#' tea
#'
#' # Get analytical blanks (tea standards)
#' tea <- subset_type(abs, "sblank")
#' get_sample_info(tea, "sample")
#'
#' # Get all blank EEMs (instrument + analytical)
#' blk <- subset_type(eem, c("iblank", "sblank"))
#' get_sample_info(blk, "sample")
#'
#' # Get non-instrument blanks
#' nonblk <- subset_type(eem, "iblank", negate = TRUE)
#' get_sample_info(nonblk, "sample")
subset_type <- function(x,
                        type = c("iblank", "sblank", "check", "sample"),
                        negate = FALSE) {
  stopifnot(.is_eemlist(x) | .is_abslist(x))

  type <- match.arg(type, several.ok = TRUE)

  x_type <- sapply(x, attr, "sample_type")

  index <- x_type %in% type


  if (negate) {
    sub_x <- x[!index]
    if (all(index)) {
      return(NULL)
    }
  } else {
    sub_x <- x[index]
    if (all(!index)) {
      return(NULL)
    }
  }

  return(sub_x)
}
