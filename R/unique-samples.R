#' Get unique EEM's or absorbance
#'
#' @param x an object of class \code{eemlist} or \code{abslist}
#' @param ... additional arguments passed to \link[base]{unique}

#' @rdname unique_samples
#' @name unique_samples
#' @method unique eemlist
#' @returns an object of class \code{eemlist} or \code{abslist} with samples with duplicate EEM's matrices (eem$x) or absorbance data (abs$data) removed.
#'
#' Note: sample names remain unchanged.
#' @export
#'
#' @examples
#' unique_eems <- unique(example_eems)
#' unique_abs <- unique(example_absorbance)

unique.eemlist <- function(x, ...){
  stopifnot(.is_eemlist(x))

  flat_X <- lapply(lapply(x, `[[`, 3), as.vector)

  # Find unique matrices based on the flattened vectors
  x <- x[!duplicated(lapply(flat_X, sort))]

  class(x) <- "eemlist"
  return(x)
}

#' @rdname unique_samples
#' @method unique abslist
#' @export
unique.abslist <- function(x, ...){
  stopifnot(.is_abslist(x))

  flat_X <- lapply(lapply(x, `[[`, 4), as.vector)

  # Find unique matrices based on the flattened vectors
  x <- x[!duplicated(lapply(flat_X, sort))]

  class(x) <- "abslist"
  return(x)
}

