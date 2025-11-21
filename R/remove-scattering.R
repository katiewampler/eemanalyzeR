#' Remove Raman and Rayleigh scattering lines
#'
#' Raman and Rayleigh scattering can cause signal interference with the sample fluorescence signal,
#' thus it is often advisable to remove them. Raman scatting is a form of inelastic scattering,
#' that is due light interacting with the solvent, shifting to longer wavelengths. Rayleigh scattering is elastic scattering, where light
#' bounces off molecules in the sample without losing energy, thus this line will always appear on the 1:1 line
#' and tends to be quite intense. Second order Raman and Rayleigh scattering may also be observed,
#' which is due to the interaction between two photons of the same energy which doubles to excitation
#' wavelength.
#'
#' @note This function is a modified version of the staRdom function \link[staRdom]{eem_rem_scat}, modified to allow
#' selective interpolation instead of all or nothing.
#'
#' @param eemlist an \code{eemlist} object containing EEM's data.
#' @param type a logical vector of length four indicating which scattering lines should be removed.
#' The order is "raman1", "raman2", "rayleigh1" and "rayleigh2" scattering.
#' Set \code{TRUE} for scattering lines that should be removed.
#' @param width a numeric vector of length four containing width of
#' scattering in nanometers to remove. The order is "raman1", "raman2", "rayleigh1" and "rayleigh2" scattering.
#' @param interpolate a logical vector of length four indicating which scattering lines to interpolate.
#' The order is "raman1", "raman2", "rayleigh1" and "rayleigh2" scattering.
#' @param method numeric 0 to 4 for interpolation method to use. Default is 1. See \link[staRdom]{eem_interp} for more info.
#' @param cores The number of cores used for parallel computation of interpolation.
#' @param arg_names Optional argument used to pass arguments from higher level functions for writing the readme.
#'
#' @importFrom staRdom eem_interp
#' @importFrom parallel detectCores
#'
#' @returns an object of class \code{eemlist}
#' @export
#' @references Gilchrist, J. R., & Reynolds, D. M. (2014). Optical Spectroscopy Instrumentation Design, Quality Assurance, and Control: Bench-Top Fluorimetry. In A. Baker, D. M. Reynolds, J. Lead, P. G. Coble, & R. G. M. Spencer (Eds.), Aquatic Organic Matter Fluorescence (pp. 147-189).
#' Cambridge: Cambridge University Press. \url{https://doi.org/10.1017/CBO9781139045452.009}
#' @examples
#' #default settings (remove all, interpolate only raman lines)
#' eemlist <- remove_scattering(example_eems)
#' plot(eemlist[[6]])
#'
#' #interpolate all
#' eemlist <- remove_scattering(example_eems, interpolate=c(TRUE,TRUE,TRUE,TRUE))
#' plot(eemlist[[6]])
#'
#' #only remove only rayleigh lines
#' eemlist <- remove_scattering(example_eems, type=c(FALSE,FALSE,TRUE,TRUE))
#' plot(eemlist[[6]])
#'
#' #change the width of the lines
#' eemlist <- remove_scattering(example_eems, width=c(16,3,100,40))
#' plot(eemlist[[6]])

remove_scattering <- function(eemlist, type = c(TRUE,TRUE,TRUE,TRUE), width=c(16,3,30,10),
                              interpolate=c(TRUE,TRUE,FALSE,FALSE), method=1,
                              cores=1, arg_names=NULL){

  stopifnot(.is_eemlist(eemlist), all(is.logical(type)), all(is.logical(interpolate)) ,
              length(width) == 4 , length(type)==4 , length(interpolate) == 4 ,
              method %in% c(0:4) , all(is.numeric(width)))

  #collect arguments for readme, and to put into the following functions
  if(is.null(arg_names)){
    args <- rlang::enquos(type, width,
                          interpolate, method,
                          cores)
    names(args) <- c("type", "width", "interpolate", "method", "cores")
  }else{args <- arg_names}

  #remove raman lines, track which are NA, either add in final or add these interpolated
  if(type[1]){raman1 <- eemR::eem_remove_scattering(eemlist, type="raman", order=1, width=width[1])}else{raman1 <- eemlist}
  if(type[2]){raman2 <- eemR::eem_remove_scattering(eemlist, type="raman", order=2, width=width[2])}else{raman2 <- eemlist}

 #remove rayleigh lines
  if(type[3]){rayleigh1 <- eemR::eem_remove_scattering(eemlist, type="rayleigh", order=1, width=width[3])}else{rayleigh1 <- eemlist}
  if(type[4]){rayleigh2 <- eemR::eem_remove_scattering(eemlist, type="rayleigh", order=2, width=width[4])}else{rayleigh2 <- eemlist}

  #get matrix of which are NA
    raman1_na <- lapply(get_sample_info(raman1, "x"), is.na)
    raman2_na <- lapply(get_sample_info(raman2, "x"), is.na)
    rayleigh1_na <- lapply(get_sample_info(rayleigh1, "x"), is.na)
    rayleigh2_na <- lapply(get_sample_info(rayleigh2, "x"), is.na)

 #combine to get fully clipped EEM
   scatter_rm <- lapply(1:length(eemlist), function(x){comb <- Reduce(`|`, list(raman1_na[[x]], raman2_na[[x]], rayleigh1_na[[x]],
                                            rayleigh2_na[[x]]))
                                            return(comb)})
   data <- lapply(1:length(eemlist), function(x){eemlist[[x]]$x[scatter_rm[[x]]] <- NA
                                                  return(eemlist[[x]])})
   class(data) <- "eemlist"

 #interpolate all areas
  if(any(interpolate == T)){data <- staRdom::eem_interp(data, cores = cores, type = method, verbose = F)}

  #reclip any areas where interpolation is not desired
   if(!(interpolate[1])){data <- lapply(1:length(data), function(x){data[[x]]$x[raman1_na[[x]]] <- NA
   return(data[[x]])})}

   if(!(interpolate[2])){data <- lapply(1:length(data), function(x){data[[x]]$x[raman2_na[[x]]] <- NA
   return(data[[x]])})}

   if(!(interpolate[3])){data <- lapply(1:length(data), function(x){data[[x]]$x[rayleigh1_na[[x]]] <- NA
   return(data[[x]])})}

   if(!(interpolate[4])){data <- lapply(1:length(data), function(x){data[[x]]$x[rayleigh2_na[[x]]] <- NA
   return(data[[x]])})}


   data <- lapply(1:length(data), function(i){
     attr(data[[i]], "is_scatter_corrected") <- TRUE
     return(data[[i]])
   })

 #return eemlist
   class(data) <- "eemlist"

 #write readme
   .write_readme_line(text="scattering lines removed via 'remove_scattering' function", slot="eem_scatter_corrected", args=args)

   return(data)

}
