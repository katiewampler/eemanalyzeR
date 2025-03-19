#TODO: add writing to readme file for dilution corretion

#' Perform Dilution Corrections
#'
#' Adjusts the EEM's and absorbance data fluorescence to account for dilutions performed prior to measurement.
#'
#' @details
#' The function uses the dilution factor provided by the metadata to dilution correct the EEMs and absorbance, because of this,
#' samples must have metadata already added to the samples using \link[eemanalyzeR]{add_metadata}.
#'
#' @param x an \code{eemlist} or \code{abslist} object. See details for more info.
#'
#' @returns an object of class \code{eemlist} or \code{abslist}
#' @export
#'
#'
#' @examples
#' eemlist <- add_metadata(metadata,example_eems)
#' eemlist <- eem_rm_blank(eemlist)
#' correct_eem <- correct_dilution(eemlist)
#'
#' abslist <- add_metadata(metadata,example_absorbance)
#' abslist <- eem_rm_blank(abslist)
#' correct_abs <- correct_dilution(abslist)

correct_dilution <- function(x){
  if(!any(.meta_added(x))){
    stop("metadata must be added to data to correct samples. \nPlease add metadata using 'add_metadata' function")
  }

  dilution_factor <- get_sample_info(x, "dilution")

  #make into fraction to use normalization function
  if(any(dilution_factor > 1)){
    dilution_factor <- 1 / dilution_factor}

  #don't correct any that have already been corrected
  dilution_factor[sapply(x, attr, "is_dil_corrected")] <- 1

  if(.is_eemlist(x)){
    res <- eem_normalize(x, dilution_factor)
  }

  if(.is_abslist(x)){
    res <- mapply(function(abs, factor){abs$data[,2] <- abs$data[,2]/factor
      return(abs)}, x, dilution_factor, SIMPLIFY = F)
  }

  res <- lapply(1:length(res), function(i){
    attr(res[[i]], "is_dil_corrected") <- TRUE
    return(res[[i]])
  })

  #TODO:add note in readme this was done

  class(res) <- class(x)
  return(res)
}
