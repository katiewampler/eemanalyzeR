#' Extract absorbance at a given wavelength
#'
#' Get the absorbance value with units of \ifelse{html}{\out{cm<sup>-1</sup>}}{\eqn{cm^-1}} at a specified wavelength with the option to
#' calculate specific absorbance (SUVA) which has units of \ifelse{html}{\out{m<sup>-1</sup>}}{\eqn{m^-1}}.
#' Note that metadata must be added in order for specific absorbance to be calculated using \link[eemanalyzeR]{add_metadata} or NA will be returned.
#'
#' If the wavelength requested is not
#' in the absorbance data, it will be interpolated first using \link[eemanalyzeR]{abs_interp}.
#'
#' @param abs an object of class \code{abs} or \code{abslist}
#' @param wl the wavelength in (nm) to get the absorbance at
#' @param cuvle cuvette (path) length in cm
#' @param suva logical, if TRUE, returns the specific absorbance
#'
#' @return A vector of absorbance values. If a value cannot be extracted, DOC01 will be returned.
#' @export
#'
#' @examples
#' abslist <- add_metadata(metadata, example_abs)
#'
#' a254 <- get_absorbance(abslist, 254)
#' suva254 <- get_absorbance(abslist, 254, suva=TRUE)

get_absorbance <- function(abs, wl, cuvle=1, suva=FALSE){
  stopifnot(.is_abs(abs) | .is_abslist(abs), is.logical(suva), is.numeric(wl), is.numeric(cuvle))

  #run across abslist
  if(.is_abslist(abs)){
    vals <- sapply(abs, get_absorbance, wl, cuvle, suva)
    return(vals)
  }

  #interpolate if needed
  if(!(wl %in% abs$data[,1])){
    abs <- abs_interp(abs)
  }

  #extract absorbance
  abs_val <- unname(abs$data[abs$data[,1] == wl,2]) *  (1/cuvle)

  #flag is not found
  if(length(abs_val) == 0){
    abs_val <- "DATA01"
  }else{
    #get specific absorbance if needed
    if(suva){
      if(.meta_added(abs)){
        abs_val <- abs_val /abs$doc_mgL * 100
      }else{
        abs_val <- "DOC01"
      }}
  }

  #if missing DOC, return code
  abs_val[is.na(abs_val)] <- "DOC01"

  return(abs_val)}

