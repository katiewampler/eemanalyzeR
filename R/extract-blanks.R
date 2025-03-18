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
#' @param info the name of the component within the \code{eem} to check for the pattern. default is 'sample'

#' @note see \link[eemR]{eem} for base \code{eem} component names and \link[eemanalyzeR]{add_metadata}
#' for extended \code{eem} component names.
#'
#' @returns an object of class \code{eemlist} either with only the blanks (\code{eem_get_blank})
#' or only the samples (\code{eem_rm_blank})
#' @export
#'
#' @rdname extract_blanks
#' @name extract_blanks
#'
#' @examples
#' blanks <- eem_get_blank(example_eems, pattern = "BEM")
#' samples <- eem_rm_blank(example_eems, pattern = "BEM")
eem_get_blank <- function(eemlist, pattern = "BEM", info="sample"){

  blank_names <- grep(pattern, get_sample_info(eemlist, info), value = T)
  if(length(blank_names) > 0){
    eemlist <- subset_samples(eemlist, info, blank_names, keep = TRUE, ignore_case = TRUE,verbose = FALSE)
  }else{
    eemlist <- list()
  }
  class(eemlist) <- "eemlist"
  return(eemlist)
}

#' @rdname extract_blanks
#' @export

eem_rm_blank <- function(eemlist, pattern= "BEM", info="sample"){
  blank_names <- grep(pattern, get_sample_info(eemlist, info), value = T)
  if(length(blank_names) > 0){
    eemlist <- subset_samples(eemlist, info, blank_names, keep = FALSE, ignore_case = TRUE,verbose = FALSE)
  }else{
    eemlist <- eemlist
  }
  class(eemlist) <- "eemlist"
  return(eemlist)
}
