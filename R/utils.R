
#Functions to check if the objects are the right class

  #' Checks if object is an eem
  #'
  #' @param eem an object
  #' @noRd
  #' @source This function was directly pulled from \link[staRdom]
  .is_eem <- function(eem) {
    ifelse(class(eem) == "eem", TRUE, FALSE)
  }

  #' Checks if object is an eemlist
  #'
  #' @param eem an object
  #' @noRd
  #' @source This function was directly pulled from \link[staRdom]
  .is_eemlist <- function(eem) {
    ifelse(class(eem) == "eemlist", TRUE, FALSE)
  }


  #' Checks if object is an abs
  #'
  #' @param abs an object
  #' @noRd
  #' @source This function was modified from \link[staRdom]
  .is_abs <- function(abs) {
    ifelse(class(abs) == "abs", TRUE, FALSE)
  }

  #' Checks if object is an abslist
  #'
  #' @param abs an object
  #' @noRd
  #' @source This function was modified from \link[staRdom]
  .is_abslist <- function(abs) {
    ifelse(class(abs) == "abslist", TRUE, FALSE)
  }


#' Get the names of abs or abslist objects
#'
#' @param abs an object of class \code{abs} or \code{abslist}
#'
#' @returns A character vector containing the names of the absorbance data
#' @export
#'
#' @examples
#' abs <- abs_dir_read(system.file("extdata", package = "eemanalyzeR"))
#' names <- abs_names(abs)
#'
#' abs_files <- list.files(system.file("extdata", package = "eemanalyzeR"),
#' full.names=TRUE, pattern="ABS")
#' abs <- abs_read(abs_files[1])
#' name <- abs_names(abs)

  abs_names <- function(abs){
    stopifnot(.is_abslist(abs) | .is_abs(abs))
    if (.is_abslist(abs)) {
      res <- unlist(lapply(abs, abs_names))
      return(res)
    }
    return(abs$sample)
  }
