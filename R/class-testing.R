#' Checks if object is the Right class
#'
#' Utility functions to test class of objects, returning a logical.
#'
#' @param x An object to test.
#'
#' @md
#'
#' @examples
#' .is_eem(example_eems)
#' .is_eemlist(example_eems)
#'
#' .is_abslist(example_abs)
#' .is_eem(example_abs)
#'
#' @noRd
#' @returns A logical value (`TRUE` or `FALSE`) indicating whether the object
#'   is of the expected class.
#'
#' @source `.is_eem` and `.is_eemlist` were directly pulled from
#'   [staRdom](https://cran.r-project.org/package=staRdom).
.is_eem <- function(x) {
  ifelse(class(x) == "eem", TRUE, FALSE)
}

.is_eemlist <- function(x) {
  ifelse(class(x) == "eemlist", TRUE, FALSE)
}

.is_abs <- function(x) {
  ifelse(class(x) == "abs", TRUE, FALSE)
}

.is_abslist <- function(x) {
  ifelse(class(x) == "abslist", TRUE, FALSE)
}
