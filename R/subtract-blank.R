#' Perform blank subtraction on EEM samples
#'
#' Subtracts the blank EEM data from sample EEM data. Samples must have
#' metadata and blank data attached prior to running.
#'
#' @param eem An `eem` or `eemlist` object.
#'
#' @return An object of class `eem` or `eemlist` where the sample data (`x`)
#' has been subtracted by the blank data (`blk_x`).
#'
#' @details
#' Blank subtraction requires that blank EEM data be attached to each sample.
#' This is done with [add_blanks()], which in turn requires metadata to be added
#' via [add_metadata()].
#'
#' @export
#' @md
#'
#' @seealso [add_metadata()]
#'
#' @examples
#' eem <- add_metadata(metadata, example_eems)
#' eem <- add_blanks(eem, validate = FALSE)
#'
#' # Subtract blank from a single EEM
#' eem_sub <- subtract_blank(eem[[1]])
#'
#' # Subtract blank from an EEM list
#' eemlist_sub <- subtract_blank(eem)
subtract_blank <- function(eem) {
  stopifnot(.is_eem(eem) | .is_eemlist(eem))

  .subtract <- function(eem) {
    if (attr(eem, "is_blank_corrected")) {
      return(eem)
    }
    eem$x <- eem$x - eem$blk_x
    attr(eem, "is_blank_corrected") <- TRUE
    return(eem)
  }

  if (.is_eemlist(eem)) {
    if (!any(.blk_added(eem))) {
      warning("Missing blank data from eem or eemlist, attempting to add using 'add_blanks' function")
      eem <- add_blanks(eem)
      warn <- TRUE
    } else {
      warn <- FALSE
    }
    eem <- lapply(eem, .subtract)
    class(eem) <- "eemlist"

    # write processing to readme
    .write_readme_line("blanks were subtracted from data via 'subtract_blank' function", "eem_blank_corrected", NULL)

    if (warn) {
      .write_readme_line("   warning: added blanks via 'add_blanks' function\n", "eem_blank_corrected", NULL)
    }

    return(eem)
  }

  if (!.blk_added(eem)) {
    stop("Missing blank data from eem or eemlist, please add using 'add_blanks' function")
  }

  # write processing to readme
  .write_readme_line("blanks were subtracted from data via 'subtract_blank' function", "eem_blank_corrected", NULL)


  return(.subtract(eem))
}
