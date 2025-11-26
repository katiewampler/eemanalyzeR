#' Calculate spectral slopes
#'
#' Computes spectral slope values using [staRdom::abs_fit_slope()], including interpolation,
#' absorption calculation, and QA/QC flagging for missing wavelengths.
#'
#' @param abs An `abs` or `abslist` object.
#' @param lim A numeric vector of length two giving the lower and upper wavelength
#'   limits used to calculate the slope.
#' @param cuvle Cuvette (path) length in cm.
#'
#' @return A vector of spectral slope values, one per sample in the `abslist`.
#' If the slope cannot be calculated, the function returns `"DATA04"`.
#'
#' @export
#' @md
#'
#' @examples
#' S275_295 <- get_abs_slope(example_abs, lim = c(275, 295))
get_abs_slope <- function(abs, lim, cuvle = 1) {
  stopifnot(.is_abs(abs) | .is_abslist(abs), is.numeric(cuvle), all(is.numeric(lim)), length(lim) == 2)

  if (.is_abslist(abs)) {
    res <- sapply(abs, get_abs_slope, lim, cuvle)
    return(res)
  }

  # interpolate if needed
  if (any(!(lim[1]:lim[2] %in% abs$data[, 1]))) {
    abs <- abs_interp(abs)
  }

  # get absorption in m^-1 (convert from absorbance to absorption based on Beer's Law)
  absorption <- abs$data[, 2] * log(10) / (cuvle / 100) # convert cm cuvette to m

  slope <- staRdom::abs_fit_slope(abs$data[, 1], absorption,
    lim = lim, l_ref = lim[1]
  )$coefficient

  # prevent non numeric values
  slope <- ifelse(is.numeric(slope), slope, "DATA04")

  return(slope)
}
