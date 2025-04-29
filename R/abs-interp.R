#' Interpolate absorbance data
#'
#' Interpolates absorbance data to 1 nanometer resolution to enable index calculation at
#' wavelengths that weren't specifically measured.
#'
#' @param abs an object of class \code{abs} or \code{abslist}
#' @param type the method used to interpolate, either "linear" for linear interpolation using \link[zoo]{na.approx} or "spline" for spline interpolation using \link[zoo]{na.spline}
#'
#' @return an object of class \code{abs} or \code{abslist}
#' @export
#'
#' @examples
#' abslist_filled <- abs_interp(example_absorbance)
#' abs_filled <- abs_interp(example_absorbance[[1]])
abs_interp <- function(abs, type = "linear"){
  stopifnot(.is_abs(abs) | .is_abslist(abs), type %in% c("linear", "spline"))

  #loop through if abslist
  if(.is_abslist(abs)){
    res <- lapply(abs, abs_interp)
    class(res) <- "abslist"
    return(res)
  }

  #extract data, and get full range of wavelengths
  abs_val <- data.frame(abs$data)
  res <- merge(abs_val, data.frame(X1=min(abs_val$X1):max(abs_val$X1)), all=T)

  #interpolate
  if(type == "linear"){res <- zoo::na.approx(res)}else if(type=="spline"){res <- zoo::na.spline(res)}

  #reformat
  abs$data <- as.matrix(res)
  abs$n <- nrow(res)
  return(abs)
 }
