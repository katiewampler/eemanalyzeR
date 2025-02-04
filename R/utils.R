
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

#' Get unique EEM's
#'
#' @param eemlist an object of class \code{eemlist}
#'
#' @returns an object of class \code{eemlist} with samples with duplicate EEM's matrices (eem$x) removed.
#'
#' Note: sample names remain unchanged.
#' @export
#'
#' @examples
#' unique_eems <- eem_unique(example_blanks)
  eem_unique <- function(eemlist){
    stopifnot(class(eemlist) == "eemlist")

    flat_X <- lapply(lapply(eemlist, `[[`, 3), as.vector)

    # Find unique matrices based on the flattened vectors
    eemlist <- eemlist[!duplicated(lapply(flat_X, sort))]

    class(eemlist) <- "eemlist"
    return(eemlist)
  }


#' Extract blanks from eemlist
#'
#' Using a regular expression, the \code{eemlist} will be cut to include either
#' only the blank samples (\code{eem_get_blank}) or remove all blank samples (\code{eem_rm_blank}).
#' This function is similar to the \link[eemR]{eem_remove_blank} function, except it is more
#' flexible to include different patterns for the blank.
#'
#' @param eemlist an object of class \code{eemlist}
#' @param pattern a character string containing a \code{\link[base]{regular expression}}
#' used to specify the sample names of the blanks.
#'
#' @returns an object of class \code{eemlist} either with only the blanks (\code{eem_get_blank})
#' or only the samples (\code{eem_rm_blank})
#' @export
#'
#' @rdname extract-blanks
#' @name extract-blanks
#'
#' @examples
#' blanks <- eem_get_blank(example_eems, pattern = "BEM")
#' samples <- eem_rm_blank(example_eems, pattern = "BEM")
 eem_get_blank <- function(eemlist, pattern){
   blank_names <- grep(pattern, eemR::eem_names(eemlist), value = T)
   eemlist <- eemR::eem_extract(eemlist, blank_names, keep = TRUE, ignore_case = TRUE,verbose = FALSE)
   class(eemlist) <- "eemlist"
   return(eemlist)
 }

#' @rdname extract-blanks
#' @export

 eem_rm_blank <- function(eemlist, pattern){
   blank_names <- grep(pattern, eemR::eem_names(eemlist), value = T)
   eemlist <- eemR::eem_extract(eemlist, blank_names, keep = FALSE, ignore_case = TRUE,verbose = FALSE)
   class(eemlist) <- "eemlist"
   return(eemlist)
 }
