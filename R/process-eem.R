#' Process and correct excitation–emission matrices (EEMs)
#'
#' Wrapper function that combines multiple EEM processing steps into a
#' streamlined workflow. The processing steps include:
#' - [subtract_blank()]: subtracts blank samples from the EEMs
#' - [remove_scattering()]: removes scattering lines with optional interpolation
#' - [ife_correct()]: corrects for inner filter effects
#' - [raman_normalize()]: normalizes fluorescence intensity to Raman units
#' - [correct_dilution()]: adjusts intensity for sample dilutions
#' - [eemR::eem_cut()]: clips the excitation–emission matrices to the region of interest
#
#'
#' @param eemlist An `eemlist` object.
#' @param abslist An `abslist` object.
#' @param ex_clip Numeric vector of length two specifying the minimum and
#'   maximum excitation wavelengths to keep.
#' @param em_clip Numeric vector of length two specifying the minimum and
#'   maximum emission wavelengths to keep.
#' @param type Logical vector of length four indicating which scattering lines to remove.
#'   The order is "raman1", "raman2", "rayleigh1", "rayleigh2".
#' @param width Numeric vector of length four specifying the width of scattering
#'   lines to remove (nm). Same order as `type`.
#' @param interpolate Logical vector of length four indicating which scattering
#'   lines to interpolate. Same order as `type`.
#' @param method Numeric (0–4) specifying the interpolation method to use.
#'   Default is 1. See [staRdom::eem_interp()] for details.
#' @param cores Integer specifying the number of cores for parallel computation
#'   during interpolation.
#' @param cuvle Cuvette (path) length in cm.
#'
#' @details
#' ## Metadata
#' Absorbance and EEM data are linked via the `meta_name` in the metadata.
#' Metadata must already be added to the samples using [add_metadata()].
#'
#' ## Tracking Processing Steps
#' Processing steps are tracked in two ways:
#' - Each sample has an attribute that records its processing steps.
#'   You can view this using `attributes(eem)`.
#' - Overall processing steps are recorded in a `readme` variable
#'   (accessible using [print_readme()]) or exported as the `readme.txt` file
#'   using [export_data()].
#'
#'
#' @return An `eemlist` object with processed and corrected EEMs.
#'
#' @export
#' @md
#'
#' @examples
#' eemlist <- add_metadata(metadata, example_eems)
#' abslist <- add_metadata(metadata, example_abs)
#' blanklist <- subset_type(eemlist, "iblank")
#' eemlist <- add_blanks(eemlist, blanklist)
#' corrected_eem <- process_eem(eemlist, abslist)
#' plot(corrected_eem)
#'
process_eem <- function(eemlist,
                        abslist,
                        ex_clip = c(247, 450),
                        em_clip = c(247, 600),
                        type = c(TRUE, TRUE, TRUE, TRUE),
                        width = c(16, 3, 30, 10),
                        interpolate = c(TRUE, TRUE, FALSE, FALSE),
                        method = 1,
                        cores = 1,
                        cuvle = 1) {
  # collect parameters for readme, and to put into the following functions
  pars <- rlang::enquos(
    ex_clip, em_clip, type, width,
    interpolate, method,
    cores, cuvle
  )
  names(pars) <- c("ex_clip", "em_clip", "type", "width", "interpolate", "method", "cores", "cuvle")

  # perform processing steps
  eemlist <- subtract_blank(eem = eemlist)
  eemlist <- remove_scattering(
    eemlist = eemlist,
    type = rlang::eval_tidy(pars$type),
    width = rlang::eval_tidy(pars$width),
    interpolate = rlang::eval_tidy(pars$interpolate),
    method = rlang::eval_tidy(pars$method),
    cores = rlang::eval_tidy(pars$cores), arg_names = pars[names(pars) %in% c("type", "width", "interpolate", "method", "cores")]
  )
  eemlist <- ife_correct(eemlist = eemlist, abslist, cuvle = rlang::eval_tidy(pars$cuvle), arg_names = pars[names(pars) %in% c("cuvle")])
  eemlist <- raman_normalize(eemlist = eemlist)
  eemlist <- correct_dilution(x = eemlist)

  # clip to just the region you care about
  ex_rm <- unique(get_sample_info(eemlist, "ex")[get_sample_info(eemlist, "ex") < ex_clip[1] | get_sample_info(eemlist, "ex") > ex_clip[2]])
  em_rm <- unique(get_sample_info(eemlist, "em")[get_sample_info(eemlist, "em") < em_clip[1] | get_sample_info(eemlist, "em") > em_clip[2]])

  eemlist <- eemR::eem_cut(eemlist, ex = ex_rm, em = em_rm, exact = T)

  # add to readme here
  .write_readme_line("EEMs data were cropped using the 'eemR::eem_cut' function", "eem_cut", args = pars[names(pars) %in% c("ex_clip", "em_clip")])

  return(eemlist)
}
