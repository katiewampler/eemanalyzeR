#' Checks if object is the Right class
#'
#' Utility functions to test class of objects, returning a logical.
#'
#' @name class_testing
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
#' @export
#' @returns A logical value (`TRUE` or `FALSE`) indicating whether the object
#'   is of the expected class.
#'
#' @source `.is_eem` and `.is_eemlist` were directly pulled from
#'   [staRdom](https://cran.r-project.org/package=staRdom).
.is_eem <- function(x) {
  ifelse(class(x) == "eem", TRUE, FALSE)
}

#' @name class_testing
#' @export
.is_eemlist <- function(x) {
  ifelse(class(x) == "eemlist", TRUE, FALSE)
}

#' @name class_testing
#' @export
.is_abs <- function(x) {
  ifelse(class(x) == "abs", TRUE, FALSE)
}

#' @name class_testing
#' @export
.is_abslist <- function(x) {
  ifelse(class(x) == "abslist", TRUE, FALSE)
}
