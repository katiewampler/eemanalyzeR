#' Calculate spectral slopes
#'
#' A wrapper for \link[staRdom]{abs_fit_slope} which interpolates the data as needed, calculates absorption, calculates the slope, and flags any missing values.
#'
#' @param abs an object of class \code{abs} or \code{abslist}
#' @param lim a vector of length two containing lower and upper limits for wavelengths to use for calculating slope
#' @param cuvle cuvette (path) length in cm
#'
#' @return A vector of spectral slope values, one for each sample in abslist. If a value cannot be extracted, DATA04 will be returned.
#' @export
#'
#' @examples
#' S275_295 <- get_abs_slope(example_absorbance, lim=c(275,295))
get_abs_slope <- function(abs, lim, cuvle=1){
  stopifnot(.is_abs(abs) | .is_abslist(abs), is.numeric(cuvle), all(is.numeric(lim)), length(lim) ==2)

  if(.is_abslist(abs)){
    res <- sapply(abs, get_abs_slope, lim, cuvle)
    return(res)
  }

  #interpolate if needed
  if(any(!(lim[1]:lim[2] %in% abs$data[,1]))){
    abs <- abs_interp(abs)
  }

  #get absorption in m^-1 (convert from absorbance to absorption based on Beer's Law)
  absorption <- abs$data[,2] * log(10) / (cuvle/100) #convert cm cuvette to m

  slope <- staRdom::abs_fit_slope(abs$data[,1], absorption,
                                  lim=lim, l_ref=lim[1])$coefficient

  #prevent non numeric values
  slope <- ifelse(is.numeric(slope), slope, "DATA04")

  return(slope)}
