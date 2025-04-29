#' Checks if object is the Right class
#'
#' Utility functions to test class of objects, returning a logical.
#'
#' @rdname class-testing
#' @name class-testing
#' @param eem an object
#'
#' @export
#' @returns a logical, \code{TRUE} or \code{FALSE} of if the object is that class
#' @source \code{.is_eem} and \code{.is_eemlist} functions were directly pulled from
#' \href{https://cran.r-project.org/web/packages/staRdom/index.html}{staRdom}.
  .is_eem <- function(eem) {
    ifelse(class(eem) == "eem", TRUE, FALSE)
  }

  #' @param eem an object
  #' @rdname class-testing
  #' @name class-testing
  #' @export
  .is_eemlist <- function(eem) {
    ifelse(class(eem) == "eemlist", TRUE, FALSE)
  }

  #' @param abs an object
  #' @rdname class-testing
  #' @name class-testing
  #' @export
  .is_abs <- function(abs) {
    ifelse(class(abs) == "abs", TRUE, FALSE)
  }

  #' @param abs an object
  #' @rdname class-testing
  #' @name class-testing
  #' @export
  .is_abslist <- function(abs) {
    ifelse(class(abs) == "abslist", TRUE, FALSE)
  }
