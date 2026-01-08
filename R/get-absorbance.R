#' Extract absorbance at a given wavelength
#'
#' Returns the absorbance value at a specified wavelength, reported in
#' \ifelse{html}{\out{cm<sup>-1</sup>}}{\eqn{cm^-1}}.
#' Optionally calculates specific absorbance (SUVA), reported in
#' \ifelse{html}{\out{m<sup>-1</sup>}}{\eqn{m^-1}}.
#'
#' @details
#' If metadata is not present, SUVA cannot be calculated and `NA` will be returned.
#' To add metadata, use [add_metadata()].
#'
#' If the wavelength requested is not
#' in the absorbance data, it will be interpolated first using
#' [abs_interp()].
#'
#' @param abs An `abs` or `abslist` object.
#' @param wl A vector of absorbance wavelengths.
#' @param cuvle Cuvette (path) length in cm.
#' @param suva If `TRUE`, returns specific absorbance (SUVA).
#'
#' @return A vector of absorbance (or specific absorbance) values.
#' If a value cannot be extracted, "DOC01" is returned.
#'
#' @export
#'
#' @examples
#' abslist <- add_metadata(metadata, example_abs)
#'
#' a254 <- get_absorbance(abslist, 254)
#' suva254 <- get_absorbance(abslist, 254, suva = TRUE)
get_absorbance <- function(abs, wl, cuvle = 1, suva = FALSE) {
  stopifnot(.is_abs(abs) | .is_abslist(abs), is.logical(suva), is.numeric(wl), is.numeric(cuvle))

  # run across abslist
  if (.is_abslist(abs)) {
    vals <- sapply(abs, get_absorbance, wl, cuvle, suva)
    return(vals)
  }

  # interpolate if needed
  if (!(wl %in% abs$data[, 1])) {
    abs <- abs_interp(abs)
  }

  # extract absorbance
  abs_val <- unname(abs$data[abs$data[, 1] == wl, 2]) * (1 / cuvle)

  # flag is not found
  if (length(abs_val) == 0) {
    abs_val <- "DATA01"
  } else {
    # get specific absorbance if needed
    if (suva) {
      if (.meta_added(abs)) {
        abs_val <- abs_val / abs$doc_mgL * 100
      } else {
        abs_val <- "DOC01"
      }
    }
  }

  # if missing DOC, return code
  abs_val[is.na(abs_val)] <- "DOC01"

  return(abs_val)
}
