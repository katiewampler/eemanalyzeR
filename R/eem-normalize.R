#' Normalize an `eem` or `eemlist` based on a normalization factor
#'
#' Useful for Raman normalization or normalizing to a maximum of one for blank comparisons.
#'
#' @param eem The `eem` or `eemlist` to normalize.
#' @param factor The normalization factor, either a single value or a vector
#' of factors. If `NULL`, it will normalize to the maximum value for each `eem`.
#'
#' @return An `eem` or `eemlist` where `x` has been normalized.
#' @export
#' @md
#'
#' @examples
#' # Normalize a single EEM
#' eem_normal <- eem_normalize(example_eems[1])
#'
#' # Normalize an entire EEM list
#' eems_normal <- eem_normalize(example_eems)
eem_normalize <- function(eem, factor = NULL) {
  if (.is_eemlist(eem)) {
    eem <- mapply(eem_normalize, eem, factor, SIMPLIFY = F)
    class(eem) <- "eemlist"
  } else {
    if (is.null(factor)) {
      factor <- max(eem$x, na.rm = T)
    }

    eem$x <- eem$x / factor
    class(eem) <- "eem"
  }

  return(eem)
}
