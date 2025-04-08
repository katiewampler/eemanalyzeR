#' Upsample EEMs and Absorbance data
#'
#' It is helpful when finding indices to have a more detailed data with wavelengths every 1 nanometer, this function can be used to
#' interpolate an eemlist or abslist to 1 nm intervals (or other interval) for this purpose.
#'
#' @param eemlist an \code{eemlist} object containing EEM's data
#' @param abslist an \code{abslist} object containing absorbance data
#' @param interval numeric, the scale at which the EEM should be upscaled to. For example: 1 will provide an interpolated EEM with values at every 1 nm.
#' @param method numeric 0 to 4 for interpolation method to use. Default is 1. See \link[staRdom]{eem_interp} for more info.
#' @param cores The number of cores used for parallel computation of interpolation.

#'
#' @importFrom staRdom eem_extend2largest eem_exclude eem_interp
#' @importFrom eemR eem_cut
#' @importFrom zoo na.spline
#' @return an \code{eemlist} or \code{abslist} with wavelengths interpolated
#' @export
#' @rdname upsample
#' @name upsample
#'
#' @note Interpolation of the absorbance data uses spline interpolation via the \link[zoo]{na.spline} function.
#'
#' @examples
#' eemlist <- example_eems
#' eemlist <- remove_scattering(eemlist)
#' eemlist_upscale <- upsample_eem(eemlist)
#' plot_eem(eemlist_upscale)
#'
#' abslist <- example_absorbance
#' abslist_upscale <- upsample_abs(abslist)
upsample_eem <- function(eemlist, interval = 1, method=1, cores=1){
  #get wavelengths of sample
  ex_orig <- unique(get_sample_info(eemlist, "ex"))
  em_orig <- unique(get_sample_info(eemlist, "em"))

  #make dummy sample with values every 1 nm
  ex <- seq(floor(min(ex_orig)), ceiling(max(ex_orig)), by=interval)
  em <- seq(floor(min(em_orig)), ceiling(max(em_orig)), by=interval)
  x <- matrix(data=NA, nrow=length(em), ncol=length(ex))
  dummy_eem <- list(file=NA, sample="interp_model", x=x, ex=ex,
                    em=em, location=NA)
  class(dummy_eem) <- "eem"

  eemlist <- append(eemlist, list(dummy_eem))
  class(eemlist) <- "eemlist"

  #use dummy sample to interpolate sample to every 1 nm
  eemlist_ext <- staRdom::eem_extend2largest(eemlist)
  eemlist_ext <- staRdom::eem_exclude(eemlist_ext, list("ex" = c(), "em"=c(), "sample"= c("interp_model")))
  eemlist_filled <- staRdom::eem_interp(eemlist_ext, type=method, cores=cores)

  eemlist_filled <- eemR::eem_cut(eemlist_filled, ex = ex_orig[!(ex_orig %in% ex)], em = em_orig[!(em_orig %in% em)], exact=T)

  return(eemlist_filled)
}


#' @rdname upsample
#' @name upsample
#' @export
upsample_abs <- function(abslist, interval = 1){
  #get wavelengths of sample
  ex_orig <- unique(get_sample_info(abslist, "data")[,1])

  #make dummy sample with values every 1 nm
  ex <- seq(floor(min(ex_orig)), ceiling(max(ex_orig)), by=interval)

  #fill in to every 1 nm
  abslist_filled <- lapply(abslist, function(x){
    abs_fill <- x$data
    missing <- ex[!(ex %in% ex_orig)]
    abs_fill <- rbind(abs_fill, matrix(data=c(missing, rep(NA,length(missing))), ncol=2, nrow=length(missing)))
    abs_fill <- abs_fill[order(abs_fill[,1], decreasing=T),]

    #fill in missing data
    abs_fill <- zoo::na.spline(abs_fill)
    x$data <- abs_fill
    return(x)
  })

  return(abslist_filled)
}
