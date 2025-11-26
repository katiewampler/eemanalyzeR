#' Get fluorescence within a specified range
#'
#' Gets fluorescence within a range of excitation and emission wavelengths and
#' returns either the maximum value or the sum across that range.
#' Missing values are interpolated using [pracma::interp2()].
#'
#' @param eem An `eem` or `eemlist` object.
#' @param ex A vector of excitation wavelengths.
#' @param em A vector of emission wavelengths.
#' @param stat A string specifying the statistic to return:
#'   - "max": return the maximum value (default)
#'   - "sum": return the sum of values
#' @param norm Logical. If `TRUE`, divides the index value by the DOC
#'   concentration. Metadata must be added using
#'   [add_metadata()], otherwise `NA` will be returned.
#'
#' @export
#' @md
#'
#' @return A numeric vector of fluorescence values. Returns `NA` if values
#'   cannot be extracted.
#'
#' @export
#'
#' @examples
#' pA <- get_fluorescence(example_eems, ex = 250:260, em = 380:480)
#' pD <- get_fluorescence(example_eems, ex = 390, em = 509)
#'
#' pA_sum <- get_fluorescence(
#'   example_eems,
#'   ex = 250:260,
#'   em = 380:480,
#'   stat = "sum"
#' )
#'
#' eemlist <- add_metadata(metadata, example_eems)
#'
#' pA_docnorm <- get_fluorescence(
#'   eemlist,
#'   ex = 250:260,
#'   em = 380:480,
#'   norm = TRUE
#' )
get_fluorescence <- function(eem, ex, em, stat = "max", norm = FALSE) {
  stopifnot(.is_eem(eem) | .is_eemlist(eem), is.numeric(ex), is.numeric(em), stat %in% c("max", "sum"), is.logical(norm))

  # apply over eemlist
  if (.is_eemlist(eem)) {
    res <- sapply(eem, get_fluorescence, ex, em, stat, norm)
    return(res)
  }

  # interpolate to get full range
  ex_p <- rep(ex, length(em)) # gives values to interpolate between
  em_p <- rep(em, length(ex)) # gives values to interpolate between
  int_res <- pracma::interp2(eem$ex, eem$em, eem$x, ex_p, em_p)

  # see if it can be calculated
  if (all(is.na(int_res))) {
    res <- "DATA01"
  } else if (stat == "max") {
    res <- max(int_res, na.rm = TRUE)
  } else if (stat == "sum") {
    res <- sum(int_res, na.rm = TRUE)
  }

  # normalize by DOC if requested
  if (norm) {
    if (res == "DATA01" | res == "NOISE01") {
      res <- res
    } else if (.meta_added(eem)) {
      res <- res / eem$doc_mgL
    } else {
      res <- "DOC01"
    }
  }

  # if missing DOC, return code
  res[is.na(res)] <- "DOC01"

  return(res)
}
