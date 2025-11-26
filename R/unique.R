#' Get unique EEMs or absorbance samples
#'
#' Removes duplicate EEM matrices or absorbance data from an `eemlist` or `abslist`.
#' Duplicates are determined based on the actual data (`eem$x` or `abs$data`) rather
#' than metadata or sample names. Only truly duplicate EEM or absorbance data are removed.
#'
#' @param x An `eemlist` or `abslist` object.
#' @param ... Additional arguments passed to [base::unique()].
#'
#' @name unique
#' @method unique eemlist
#'
#' @return An object of class `eemlist` or `abslist` with duplicate samples removed.
#'
#' @details
#' The first duplicate will be retained. The sample name will not be updated but
#' will reflect the give name of the first duplicated sample.
#'
#' @export
#' @md
#'
#' @examples
#' # Remove duplicate EEMs
#' unique_eems <- unique(example_eems)
#'
#' # Remove duplicate absorbance data
#' unique_abs <- unique(example_abs)
unique.eemlist <- function(x, ...) {
  stopifnot(.is_eemlist(x))

  flat_X <- lapply(lapply(x, `[[`, 3), as.vector)

  # Find unique matrices based on the flattened vectors
  x <- x[!duplicated(lapply(flat_X, sort))]

  class(x) <- "eemlist"
  return(x)
}

#' @rdname unique
#' @method unique abslist
#' @export
unique.abslist <- function(x, ...) {
  stopifnot(.is_abslist(x))

  flat_X <- lapply(lapply(x, `[[`, 4), as.vector)

  # Find unique matrices based on the flattened vectors
  x <- x[!duplicated(lapply(flat_X, sort))]

  class(x) <- "abslist"
  return(x)
}
