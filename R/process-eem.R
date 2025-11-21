#' Process and correct excitation emission matrices
#'
#' A wrapper to combine all the processing functions together to make a cleaner workflow.
#' Processing steps include:
#' \itemize{
#' \item{\link[eemanalyzeR]{subtract_blank}: which subtracts the blanks from the samples}
#' \item{\link[eemanalyzeR]{remove_scattering}: which removes scattering lines with optional interpolation}
#' \item{\link[eemanalyzeR]{ife_correct}: which corrects the fluorescence intensity for inner filter effects}
#' \item{\link[eemanalyzeR]{raman_normalize}: which normalizes the fluorescence intensity to the raman unit area}
#' \item{\link[eemanalyzeR]{correct_dilution}: which corrects the fluorescence intensity for any dilutions}
#' \item{\link[eemR]{eem_cut}: which clips the area of the excitation emission matrices to the area of interest}
#' }
#'
#' @param eemlist an \code{eemlist} object containing EEM's data. See details for more info.
#' @param abslist an \code{abslist} object containing absorbance data.
#' @param ex_clip numerical vector of length two with the minimum and maximum wavelengths to clip the excitation wavelengths to.
#' @param em_clip numerical vector of length two with the minimum and maximum wavelengths to clip the emission wavelengths to.
#' @param type a logical vector of length four indicating which scattering lines should be removed.
#' The order is "raman1", "raman2", "rayleigh1" and "rayleigh2" scattering.
#' Set \code{TRUE} for scattering lines that should be removed.
#' @param width a numeric vector of length four containing width of
#' scattering in nanometers to remove. The order is "raman1", "raman2", "rayleigh1" and "rayleigh2" scattering.
#' @param interpolate a logical vector of length four indicating which scattering lines to interpolate.
#' The order is "raman1", "raman2", "rayleigh1" and "rayleigh2" scattering.
#' @param method numeric 0 to 4 for interpolation method to use. Default is 1. See \link[staRdom]{eem_interp} for more info.
#' @param cores The number of cores used for parallel computation of interpolation.
#' @param pathlength a numeric value indicating the pathlength (in cm) of the
#'   cuvette used for absorbance measurement. Default is 1 (1 cm).
#'
#' @details
#' The function links the absorbance and EEM's data by the metadata
#' name which should be the same between the two datasets, because of this,
#' samples must have metadata already added to the samples using \link[eemanalyzeR]{add_metadata}.
#'
#'
#' @return an object of class \code{eemlist}
#' @export
#'
#' @examples
#' eemlist <- add_metadata(metadata, example_eems)
#' abslist <- add_metadata(metadata, example_abs)
#' eemlist <- add_blanks(eemlist, validate=FALSE)
#' corrected_eem <- process_eem(eemlist, abslist)
#' plot(corrected_eem)
#'
process_eem <- function(eemlist, abslist, ex_clip = c(247,450),
                        em_clip = c(247,600), type = c(TRUE,TRUE,TRUE,TRUE), width=c(16,3,30,10),
                        interpolate=c(TRUE,TRUE,FALSE,FALSE), method=1,
                        cores=1, pathlength=1){

  #collect parameters for readme, and to put into the following functions
  pars <- rlang::enquos(ex_clip,em_clip, type, width,
                        interpolate, method,
                        cores, pathlength)
  names(pars) <- c("ex_clip", "em_clip", "type", "width", "interpolate", "method", "cores", "pathlength")

  #perform processing steps
  eemlist <- subtract_blank(eem = eemlist)
  eemlist <- remove_scattering(eemlist = eemlist,
                               type = rlang::eval_tidy(pars$type),
                               width=rlang::eval_tidy(pars$width),
                               interpolate=rlang::eval_tidy(pars$interpolate),
                               method=rlang::eval_tidy(pars$method),
                               cores=rlang::eval_tidy(pars$cores), arg_names = pars[names(pars) %in% c("type", "width", "interpolate", "method", "cores")])
  eemlist <- ife_correct(eemlist = eemlist, abslist, pathlength = rlang::eval_tidy(pars$pathlength), arg_names = pars[names(pars) %in% c("pathlength")])
  eemlist <- raman_normalize(eemlist = eemlist)
  eemlist <- correct_dilution(x = eemlist)

  #TODO: (make new wrapper to incorporate readme)
  #clip to just the region you care about
  ex_rm <- unique(get_sample_info(eemlist, "ex")[get_sample_info(eemlist, "ex") < ex_clip[1] | get_sample_info(eemlist, "ex") > ex_clip[2]])
  em_rm <- unique(get_sample_info(eemlist, "em")[get_sample_info(eemlist, "em") < em_clip[1] | get_sample_info(eemlist, "em") > em_clip[2]])

  eemlist <- eemR::eem_cut(eemlist, ex=ex_rm, em=em_rm, exact=T)

  #add to readme here
  .write_readme_line("EEMs data were cropped using the 'eemR::eem_cut' function", "eem_cut", args = pars[names(pars) %in% c("ex_clip", "em_clip")])

  return(eemlist)
}
