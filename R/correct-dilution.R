#TODO: add writing to readme file for dilution corretion

#' Perform Dilution Corrections
#'
#' Adjusts the EEM's data fluorescence to account for dilutions performed prior to measurement.
#'
#' @details
#' The function uses the dilution factor provided by the metadata to dilution correct the EEMs, because of this,
#' samples must have metadata already added to the samples using \link[eemanalyzeR]{add_metadata}.
#'
#' @param eemlist an \code{eemlist} object containing EEM's data. See details for more info.
#'
#' @returns an object of class \code{eemlist}
#' @export
#'
#'
#' @examples
#' eemlist <- add_metadata(metadata,example_eems)
#' eemlist <- eem_rm_blank(eemlist)
#' correct_eem <- correct_dilution(eemlist)

correct_dilution <- function(eemlist){
  if(!any(.meta_added(eemlist))){
    stop("metadata must be added to eemlist to correct samples. \nPlease add metadata using 'add_metadata' function")
  }

  dilution_factor <- get_sample_info(eemlist, "dilution")

  #make into fraction to use normalization function
  if(any(dilution_factor > 1)){
    dilution_factor <- 1 / dilution_factor}

  #don't correct any that have already been corrected
  dilution_factor[sapply(eemlist, attr, "is_dil_corrected")] <- 1

  res <- eem_normalize(eemlist, dilution_factor)

  res <- lapply(1:length(res), function(i){
    attr(res[[i]], "is_dil_corrected") <- TRUE
    return(res[[i]])
  })

  #TODO:add note in readme this was done

  class(res) <- "eemlist"
  return(res)
}
