
#' Checks if the eems or absorbance has had metadata added
#' @noRd
.meta_added <- function(x){
  stopifnot(class(x) %in% c("eemlist", "eem", "abs", "abslist"))

  if(inherits(x, c("eem","abs"))){
    items <- names(x)
    augment_names <- c("meta_name","dilution","analysis_date", "description","doc_mgL","notes")
    augmented <- all(augment_names %in% items)
  }else{
    augmented <- unlist(lapply(x, .meta_added))
  }
  return(augmented)
}

#' Checks if the eems or absorbance has had blank added
#' @noRd
.blk_added <- function(x){
  stopifnot(class(x) %in% c("eemlist", "eem", "abs", "abslist"))

  if(inherits(x, c("eem","abs"))){
    items <- names(x)
    augment_names <- c("blk_file", "blk_x")
    augmented <- all(augment_names %in% items)
  }else{
    augmented <- unlist(lapply(x, .blk_added))
  }
  return(augmented)

}

#' Removes extra list items from eemlist, replaces sample with meta_name for matching
#' @noRd
# TODO running an eemlist through this function removes the metadata added attribute.
# Need to keep metadata after making the base eem
# Also this is never called by another function as far as I can tell.
.make_base_eem <- function(x){
  if(.is_eemlist(x)){
    x <- lapply(x, .make_base_eem)
    class(x) <- "eemlist"
    return(x)
  }

  if(.meta_added(x)){
    x$sample <- x$meta_name
    x$meta_name <- NULL
    x$dilution <- NULL
    x$integration_time_s <- NULL
    x$raman_area_1s <- NULL
    x$analysis_date <- NULL
    x$description <- NULL
    x$doc_mgL <-NULL
    x$notes <- NULL
    x$blk_x <- NULL
    x$blk_file <- NULL
  }
  return(x)

}

#' Check which processing steps have been completed on an eem or eemlist
#'
#' @param eem an \code{eemlist} or \code{eem} object containing EEM's data.
#' @noRd
#' @return a data.frame where the first column is the different attributes and the second is a T/F where F indicates that not all the samples
#' have been processed for that attribute.
#'
check_processing <- function(eem){
  stopifnot(.is_eemlist(eem) | .is_eem(eem))
  steps <- data.frame(attr = c("is_blank_corrected", "is_scatter_corrected","is_ife_corrected",
                               "is_raman_normalized", "is_dil_corrected", "is_doc_normalized"),
                      warning = c("blank corrected", "scattering removed", "ife corrected",
                                  "raman normalized", "dilution corrected", "DOC normalized"),
                      func = c("subtract_blank", "remove_scattering", "ife_correct",
                               "raman_normalized", "correct_dilution", "eem_normalize"),
                      done = FALSE)

  if(.is_eemlist(eem)){
    steps$done <- apply(steps, 1, function(r){done = any(sapply(eem, attr, r[1]))})
  }

  if(.is_eem(eem)){
    steps$done <- apply(steps, 1, function(r){done = attr(eem, r[1])})
  }


  return(steps)
}
