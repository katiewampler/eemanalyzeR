#' Get Function for Calculating Signal to Noise
#'
#' @param method currently only accepts "sqrt" which uses
#' Horiba's Square Root Method (\link[eemanalyzeR]{calc-SNR})
#'
#' @noRd
#' @returns a function used to get signal to noise ratio
#'
get_SNR_function <- function(method="sqrt"){

  if(is.function(method)){
    return(method)
  }

  switch(method,
         "sqrt" = calc_raman_SNR_sqrt,
         stop(method, " is not a known function to get signal to noise\n  to create your own see vingette browseVingettes('eemanalyzeR')"))
}

#' Get Signal to Noise Ratio for EEMs
#'
#' @param eem an \code{eem} or \code{eemlist} object containing EEM's data
#' @param method method used to get signal to noise, either a custom function or
#' "sqrt", the default. See \link[eemanalyzeR]{calc_raman_SNR_sqrt} for details.
#'
#' @returns a vector of signal to noise ratios for each sample in the \code{eem} or \code{eemlist}
#' @export
#'
#' @examples
#' eemlist <- add_metadata(metadata, example_eems)
#' eemlist <- add_blanks(eemlist, validate=FALSE)
#' SNR <- get_SNR(eemlist)
#'
get_SNR <- function(eem, method="sqrt"){
  stopifnot(.is_eemlist(eem) | .is_eem(eem))

  SNRmethod <- get_SNR_function(method)

  if(.is_eemlist(eem)){
    res <- sapply(eem, get_SNR, method)
    return(res)
  }

  SNR <- SNRmethod(eem)
  return(SNR)
}
