#' Get fluorescence within a specified range
#'
#' Gets the fluorescence within a range of excitation and emission wavelengths and returns the maximum or sum within that range.
#' Missing values are interpolated using \link[pracma]{interp2}.
#'
#'
#' @param eem an \code{eem} or \code{eemlist} object containing EEM's data
#' @param ex a vector of excitation wavelengths
#' @param em a vector of emission wavelengths
#' @param stat either "max" to get the maximum value within the specified range or "sum" to get the sum across the specified range
#' @param norm logical, if TRUE will divide index value by the DOC concentration. Note that metadata must be added if TRUE using \link[eemanalyzeR]{add_metadata} or NA will be returned.
#' @param SNR_method method used to find signal to noise ratio see \link[eemanalyzeR]{get_SNR} for more details.
#' @return A vector of fluorescence values. If a value cannot be extracted, NA will be returned.
#' @export
#'
#' @examples
#' pA <- get_fluorescence(example_eems, ex=250:260, em=380:480)
#' pD <- get_fluorescence(example_eems, ex=390, em=509)
#' pA_sum <- get_fluorescence(example_eems, ex=250:260, em=380:480, stat="sum")
#'
#' eemlist <- add_metadata(metadata, example_eems)
#' pA_docnorm <- get_fluorescence(eemlist, ex=250:260, em=380:480, norm=TRUE)
#'
#'
get_fluorescence <- function(eem, ex, em, stat="max", norm=FALSE, SNR_method="sqrt"){
  stopifnot(.is_eem(eem) | .is_eemlist(eem), is.numeric(ex), is.numeric(em), stat %in% c("max","sum"), is.logical(norm))

  #apply over eemlist
  if(.is_eemlist(eem)){
    res <- sapply(eem, get_fluorescence, ex, em, stat, norm, SNR_method)
    return(res)
  }

  #interpolate to get full range
  ex_p <- rep(ex, length(em)) #gives values to interpolate between
  em_p <- rep(em, length(ex)) #gives values to interpolate between
  int_res <- pracma::interp2(eem$ex, eem$em, eem$x, ex_p, em_p)

  #see if it can be calculated
  if(all(is.na(int_res))){
    res <- "DATA01"
  }else if(stat == "max"){
    res <- max(int_res, na.rm=TRUE)
  }else if(stat == "sum"){
    res <- sum(int_res, na.rm = TRUE)
  }

  #check SNR
  if(.blk_added(eem)){
    snr <- get_SNR(eem, method=SNR_method)
    res <- ifelse(res < snr, "NOISE01", res)
  }

  #normalize by DOC if requested
  if(norm){
    if(res == "DATA01"|res == "NOISE01"){
      res <- res
    }else if(.meta_added(eem)){
      res <- res / eem$doc_mgL
    }else{
      res <- "DOC01"
    }}

  #if missing DOC, return code
  res[is.na(res)] <- "DOC01"

  return(res)
}
