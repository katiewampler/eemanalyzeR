#' Remove Raman and Rayleigh scattering lines from EEMs
#'
#' Raman and Rayleigh scattering can interfere with sample fluorescence signals.
#' Raman scattering is inelastic scattering caused by interaction with the solvent,
#' shifting to longer wavelengths. Rayleigh scattering is elastic scattering, where
#' light bounces off molecules without energy loss, appearing on the 1:1 line and
#' often intense. Second-order Raman and Rayleigh scattering may also occur due to
#' two-photon interactions doubling the excitation wavelength.
#'
#' @note This function is a modified version of [staRdom::eem_rem_scat()]
#'   from the `staRdom` package, allowing selective interpolation instead of all-or-nothing.
#'
#' @param eemlist An `eemlist` object.
#' @param type Logical vector of length four indicating which scattering lines to remove.
#'   The order is "raman1", "raman2", "rayleigh1", "rayleigh2". Set `TRUE` to remove.
#' @param width Numeric vector of length four specifying the width (nm) of the scattering lines to remove.
#'   Order is "raman1", "raman2", "rayleigh1", "rayleigh2".
#' @param interpolate Logical vector of length four indicating which scattering lines to interpolate.
#'   Order is "raman1", "raman2", "rayleigh1", "rayleigh2".
#' @param method Numeric 0–4 specifying interpolation method (default = 1). See [`eem_interp()`][staRdom::eem_interp].
#' @param cores Number of cores for parallel interpolation computation.
#' @param arg_names Optional list of arguments passed from higher-level
#'   functions for README generation.
#' @return An `eemlist` object with specified scattering lines removed.
#'
#' @export
#' @md
#'
#' @references Gilchrist, J. R., & Reynolds, D. M. (2014). Optical Spectroscopy Instrumentation Design,
#' Quality Assurance, and Control: Bench-Top Fluorimetry. In A. Baker, D. M. Reynolds, J. Lead, P. G. Coble,
#' & R. G. M. Spencer (Eds.), Aquatic Organic Matter Fluorescence (pp. 147-189).
#' Cambridge: Cambridge University Press. <https://doi.org/10.1017/CBO9781139045452.009>
#'
#' @examples
#' # default settings (remove all, interpolate only raman lines)
#' eemlist <- remove_scattering(example_eems)
#' plot(eemlist[[6]])
#'
#' # interpolate all
#' eemlist <- remove_scattering(example_eems, interpolate = c(TRUE, TRUE, TRUE, TRUE))
#' plot(eemlist[[6]])
#'
#' # only remove only rayleigh lines
#' eemlist <- remove_scattering(example_eems, type = c(FALSE, FALSE, TRUE, TRUE))
#' plot(eemlist[[6]])
#'
#' # change the width of the lines
#' eemlist <- remove_scattering(example_eems, width = c(16, 3, 100, 40))
#' plot(eemlist[[6]])
remove_scattering <- function(eemlist, type = c(TRUE, TRUE, TRUE, TRUE), width = c(16, 3, 30, 10),
                              interpolate = c(TRUE, TRUE, FALSE, FALSE), method = 1,
                              cores = 1, arg_names = NULL) {
  stopifnot(
    .is_eemlist(eemlist), all(is.logical(type)), all(is.logical(interpolate)),
    length(width) == 4, length(type) == 4, length(interpolate) == 4,
    method %in% c(0:4), all(is.numeric(width))
  )

  # collect arguments for readme, and to put into the following functions
  if (is.null(arg_names)) {
    args <- rlang::enquos(
      type, width,
      interpolate, method,
      cores
    )
    names(args) <- c("type", "width", "interpolate", "method", "cores")
  } else {
    args <- arg_names
  }

  # remove raman lines, track which are NA, either add in final or add these interpolated
  if (type[1]) {
    raman1 <- eemR::eem_remove_scattering(eemlist, type = "raman", order = 1, width = width[1])
  } else {
    raman1 <- eemlist
  }
  if (type[2]) {
    raman2 <- eemR::eem_remove_scattering(eemlist, type = "raman", order = 2, width = width[2])
  } else {
    raman2 <- eemlist
  }

  # remove rayleigh lines
  if (type[3]) {
    rayleigh1 <- eemR::eem_remove_scattering(eemlist, type = "rayleigh", order = 1, width = width[3])
  } else {
    rayleigh1 <- eemlist
  }
  if (type[4]) {
    rayleigh2 <- eemR::eem_remove_scattering(eemlist, type = "rayleigh", order = 2, width = width[4])
  } else {
    rayleigh2 <- eemlist
  }

  # get matrix of which are NA
  raman1_na <- lapply(get_sample_info(raman1, "x"), is.na)
  raman2_na <- lapply(get_sample_info(raman2, "x"), is.na)
  rayleigh1_na <- lapply(get_sample_info(rayleigh1, "x"), is.na)
  rayleigh2_na <- lapply(get_sample_info(rayleigh2, "x"), is.na)

  # combine to get fully clipped EEM
  scatter_rm <- lapply(1:length(eemlist), function(x) {
    comb <- Reduce(`|`, list(
      raman1_na[[x]], raman2_na[[x]], rayleigh1_na[[x]],
      rayleigh2_na[[x]]
    ))
    return(comb)
  })
  data <- lapply(1:length(eemlist), function(x) {
    eemlist[[x]]$x[scatter_rm[[x]]] <- NA
    return(eemlist[[x]])
  })
  class(data) <- "eemlist"

  # interpolate all areas
  if (any(interpolate == T)) {
    data <- staRdom::eem_interp(data, cores = cores, type = method, verbose = F)
  }

  # reclip any areas where interpolation is not desired
  if (!(interpolate[1])) {
    data <- lapply(1:length(data), function(x) {
      data[[x]]$x[raman1_na[[x]]] <- NA
      return(data[[x]])
    })
  }

  if (!(interpolate[2])) {
    data <- lapply(1:length(data), function(x) {
      data[[x]]$x[raman2_na[[x]]] <- NA
      return(data[[x]])
    })
  }

  if (!(interpolate[3])) {
    data <- lapply(1:length(data), function(x) {
      data[[x]]$x[rayleigh1_na[[x]]] <- NA
      return(data[[x]])
    })
  }

  if (!(interpolate[4])) {
    data <- lapply(1:length(data), function(x) {
      data[[x]]$x[rayleigh2_na[[x]]] <- NA
      return(data[[x]])
    })
  }


  data <- lapply(1:length(data), function(i) {
    attr(data[[i]], "is_scatter_corrected") <- TRUE
    return(data[[i]])
  })

  # return eemlist
  class(data) <- "eemlist"

  # write readme
  .write_readme_line(text = "scattering lines removed via 'remove_scattering' function", slot = "eem_scatter_corrected", args = args)

  return(data)
}
