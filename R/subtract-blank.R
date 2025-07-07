#' Perform blank subtraction on EEM samples
#'
#' @param eem an \code{eem} or \code{eemlist} that has been augmented with metadata and blanks. See details for more info.
#'
#' @returns an object of class \code{eem} or \code{eemlist} where the sample data (\code{x})
#' has been subtracted by the blank data (\code{blk_x})
#'
#' @details
#' In order to subtract the blank EEMs data from the sample data, you must first 'attach' the blank data to the \code{eem}
#' by using the function \link[eemanalyzeR]{add_blanks}. Note that in order to run \link[eemanalyzeR]{add_blanks} you
#' first need to add the metadata via \link[eemanalyzeR]{add_metadata}.
#'
#' The processing steps are tracked in two ways:
#' \itemize{
#'  \item{processing is tracked for each sample via an attribute attached to each \code{eem}
#' object. You can visualize this using the function \link[base]{attributes}\code{(eem)}}
#' \item{overall processing steps are also tracked and output as a readme.txt file in the processed data folder} ## TO DO: update when this is finalized
#' }
#' @export
#'
#' @examples
#' eem <- add_metadata(metadata,example_eems)
#' eem <- add_blanks(eem, validate=FALSE)
#' eem_sub <- subtract_blank(eem[[1]])
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
    }else{warn <-FALSE}
    eem <- lapply(eem, .subtract)
    class(eem) <- "eemlist"

    #write processing to readme
    .write_readme_line("blanks were subtracted from data via 'subtract_blank' function",  "eem_blank_corrected", NULL)

    if(warn){
      .write_readme_line("   warning: added blanks via 'add_blanks' function\n", "eem_blank_corrected", NULL)}

    return(eem)
  }

  if (!.blk_added(eem)) {
    stop("Missing blank data from eem or eemlist, please add using 'add_blanks' function")
  }

  #write processing to readme
  .write_readme_line("blanks were subtracted from data via 'subtract_blank' function", "eem_blank_corrected", NULL)


  return(.subtract(eem))
}
