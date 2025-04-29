#' QA/QC Flags for Missing Data
#'
#' Checks if a metric can't be calculated or may be inaccurate due to missing wavelengths required to calculate that metric.
#'
#' @param x an \code{eemlist}, \code{eem}, \code{abslist}, or \code{abs} object.
#' @param ex a vector of the excitation wavelengths required to calculate index, only required if x is an \code{eemlist} or \code{eem}
#' @param em a vector of the emission wavelengths required to calculate index, only required if x is an \code{eemlist} or \code{eem}
#' @param wl a vector of the wavelengths required to calculate index, only required if x is an \code{abslist} or \code{abs}
#' @param all logical, does the index still get calculated if some wavelengths are missing? used to determine if DATA_01 or DATA_02 is returned
#'
#' @returns a vector containing text flags and NA values, the following values may be returned:
#' \itemize{
#'  \item DATA_01: Missing data required to calculate the index
#'  \item DATA_02: Missing some wavelengths required to calculate the index, value may be inaccurate
#'  \item NA: No missing data, no flag needed for missing data
#' }
#' @export
#'
#' @examples
#' #checking absorbance data
#'   flag_missing(example_absorbance, wl=400) #data exists
#'   flag_missing(example_absorbance, wl=100) #data doesn't exist
#'   flag_missing(example_absorbance, wl=100:254, all=FALSE) #some data exists, still calculate
#'
#' #checking fluorescence data
#'   flag_missing(example_eems, ex=270:280, em=300:320) #data exists
#'   flag_missing(example_eems, ex=100:150, em=300:320) #data doesn't exist
#'   flag_missing(example_eems, ex=100:350, em=300:320, all=FALSE) #some data exists, still calculate
flag_missing <- function(x, ex=NULL, em = NULL, wl=NULL, all=TRUE){
  stopifnot(.is_abs(x) | .is_abslist(x) | .is_eem(x) | .is_eemlist(x), !all(is.null(c(wl, ex, em))))

  if(.is_eemlist(x)){
    flags <- sapply(x, flag_missing, ex=ex, em=em, all=all)
    return(flags)}

  if(.is_abslist(x)){
    flags <- sapply(x, flag_missing, wl=wl, all=all)
    return(flags)}

  if(.is_abs(x)){
    #get ranges of wavelengths in data
    range <- min(x$data[,1]):max(x$data[,1])

    #is the range completely contained in ranges?
      if(all(wl %in% range)){
        flag <- NA
      }else if(any(wl %in% range) & all == FALSE){
        flag <- "DATA_02" #some wavelengths in data, may not be accurate
      }else{
        flag <- "DATA_01" #index range not in data, unable to report value
      }}



  if(.is_eem(x)){
    #get ranges of wavelengths in data
    ex_range <- min(x$ex):max(x$ex)
    em_range <- ceiling(min(x$em)):floor(max(x$em)) #round because they're usually not integers

    #check if fully in range and return flag if needed
      if(all(ex %in% ex_range) & all(em %in% em_range)){
        flag <- NA
      }else if(any(ex %in% ex_range) & any(em %in% em_range) & all == FALSE){
        flag <- "DATA_02" #entire index range not contained in data
      }else{
        flag <- "DATA_01" #index range not in data, unable to report value
      }}

  return(flag)
}

