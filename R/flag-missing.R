#' QA/QC flags for missing data
#'
#' Checks if a metric can't be calculated or may be inaccurate due to missing
#' wavelengths required to calculate that metric.
#'
#' @param x An `eemlist`, `eem`, `abslist`, or `abs` object.
#' @param ex A vector of excitation wavelengths required to calculate the index.
#'   Only needed if `x` is an `eemlist` or `eem`.
#' @param em A vector of emission wavelengths required to calculate the index.
#'   Only needed if `x` is an `eemlist` or `eem`.
#' @param wl A vector of wavelengths required to calculate the index.
#'   Only needed if `x` is an `abslist` or `abs`.
#' @param all If `TRUE` index will not be calculated
#'   if some wavelengths are missing, returning "DATA01".
#'
#' @return A vector containing text flags and `NA` values. Possible values:
#'
#' - "DATA_01": Missing data required to calculate the index.
#' - "DATA_02": Missing some wavelengths required to calculate the index; value
#'   may be inaccurate.
#' - `NA`: No missing data; no flag needed.
#'
#' @export
#' @md
#'
#' @examples
#' # checking absorbance data
#' # data exists
#' flag_missing(example_abs, wl = 400)
#'
#' # data doesn't exist
#' flag_missing(example_abs, wl = 100)
#'
#' # some data exists, still calculate
#' flag_missing(example_abs, wl = 100:254, all = FALSE)
#'
#' # checking fluorescence data
#' # data exists
#' flag_missing(example_eems, ex = 270:280, em = 300:320)
#'
#' # data doesn't exist
#' flag_missing(example_eems, ex = 100:150, em = 300:320)
#'
#' # some data exists, still calculate
#' flag_missing(example_eems, ex = 100:350, em = 300:320, all = FALSE)
flag_missing <- function(x, ex = NULL, em = NULL, wl = NULL, all = TRUE) {
  stopifnot(.is_abs(x) | .is_abslist(x) | .is_eem(x) | .is_eemlist(x), !all(is.null(c(wl, ex, em))))

  if (.is_eemlist(x)) {
    flags <- sapply(x, flag_missing, ex = ex, em = em, all = all)
    return(flags)
  }

  if (.is_abslist(x)) {
    flags <- sapply(x, flag_missing, wl = wl, all = all)
    return(flags)
  }

  if (.is_abs(x)) {
    # get ranges of wavelengths in data
    range <- min(x$data[, 1]):max(x$data[, 1])

    # is the range completely contained in ranges?
    if (all(wl %in% range)) {
      flag <- NA
    } else if (any(wl %in% range) & all == FALSE) {
      flag <- "DATA02" # some wavelengths in data, may not be accurate
    } else {
      flag <- "DATA01" # index range not in data, unable to report value
    }
  }


  if (.is_eem(x)) {
    # get ranges of wavelengths in data
    ex_range <- min(x$ex):max(x$ex)
    em_range <- ceiling(min(x$em)):floor(max(x$em)) # round because they're usually not integers

    # check if fully in range and return flag if needed
    if (all(ex %in% ex_range) & all(em %in% em_range)) {
      flag <- NA
    } else if (any(ex %in% ex_range) & any(em %in% em_range) & all == FALSE) {
      flag <- "DATA02" # entire index range not contained in data
    } else {
      flag <- "DATA01" # index range not in data, unable to report value
    }
  }

  return(flag)
}
