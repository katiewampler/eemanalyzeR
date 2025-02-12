#' Subset eemlist or abslist using components
#'
#' Helper function that build upon \link[eemR]{eem_extract} function. Used to select or remove samples based on the info extracted by
#' \link[eemanalyzeR]{get_sample_info} allowing selection of samples based on components besides just the sample name.
#'
#' @param x an object of class \code{eemlist} or \code{abslist}
#' @param sample a vector of the names or other info to use to select EEM's from \code{eemlist} or absorbance from \code{abslist}
#' @param info the name of the component within the \code{eem} or \code{abs} to extract.
#' see \link[eemR]{eem} for base \code{eem} component names and \link[eemanalyzeR]{abs_read} for base \code{abs} names.
#' see \link[eemanalyzeR]{add_metadata} for extended \code{eem} component names.
#' @param keep logical. If TRUE, the specified sample will be returned. If FALSE, they will be removed.
#' @param ignore_case logical, should sample name case should be ignored (TRUE) or not (FALSE). Default is FALSE.
#' @param verbose logical determining if removed/extracted eems should be printed on screen.
#'
#' @returns
#' An object of class \code{eemlist} or \code{abslist} with the samples excluded or selected based on the 'sample' vector.
#' @export
#' @examples
#'  #subset by name
#' names <- get_sample_info(example_eems, "sample")
#' eem_subset <- subset_samples(example_eems, "sample", names[1]) #default is to remove
#' #but use keep=T to keep instead
#' eem_subset <- subset_samples(example_eems,"sample", names[1], keep=TRUE)
#'
#' #subset by file_name
#' eemlist <- add_metadata(metadata, example_eems)
#' names <- get_sample_info(eemlist, "meta_name")
#' eem_subset <- subset_samples(eemlist, "meta_name", names[1]) #default is to remove
#'

subset_samples <- function(x, info, sample, keep=F, ignore_case=F,
                           verbose=T){
  stopifnot(inherits(x, "eemlist") | inherits(x, "abslist"), info %in% unlist(lapply(x,names)))
  values <- get_sample_info(x, info)
  sample <- gsub("([\\(\\)\\-])", "\\\\\\1", sample) #escape special characters
  to_remove <- grepl(paste(sample, collapse = "|"), values,
                     ignore.case = ignore_case)
  x[xor(to_remove, keep)] <- NULL
  if (verbose) {
    if (all(to_remove == FALSE)) {
      message("Nothing to remove.")
    }
    else {
      message(ifelse(keep, "Extracted sample(s):", "Removed sample(s):"),
          values[to_remove], "\n")
    }
  }
  return(x)
}
