#TODO: add writing to readme file for raman noramlization

#' Perform Raman Normalization
#'
#' Normalizes the EEM intensity to a standard scale of Raman Units (R.U.) based
#' on the area of the water Raman peak (Lawaetz and Steadmon 2009).
#'
#' @details
#' The function uses the raman area provided by the metadata to normalize the EEMs, because of this,
#' samples must have metadata already added to the samples using \link[eemanalyzeR]{add_metadata}.
#'
#' @param eemlist an \code{eemlist} object containing EEM's data. See details for more info.
#'
#' @returns an object of class \code{eemlist}
#' @export
#'
#' @source
#' Lawaetz, A. J., & Stedmon, C. A. (2009). Fluorescence Intensity Calibration Using
#' the Raman Scatter Peak of Water. Applied Spectroscopy, 63(8), 936-940.
#' \href{https://journals.sagepub.com/doi/10.1366/000370209788964548}{https://journals.sagepub.com/doi/10.1366/000370209788964548}
#'
#' @examples
#' eemlist <- add_metadata(metadata,example_eems)
#' eemlist <- eem_rm_blank(eemlist)
#' correct_eem <- raman_normalize(eemlist)

raman_normalize <- function(eemlist){
  if(!any(.meta_added(eemlist))){
    stop("metadata must be added to eemlist and abslist to link samples. \nPlease add metadata using 'add_metadata' function")
  }

  raman_1s <- get_sample_info(eemlist, "raman_area_1s")
  int_time_s <- get_sample_info(eemlist, "integration_time_s")
  raman_factor <- raman_1s * int_time_s
  res <- eem_normalize(eemlist, raman_factor)

}
