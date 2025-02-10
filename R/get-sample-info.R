#' Extract info from eemlist or abslist
#'
#' Helper function that build upon the \link[eemR]{eem_names}
#' function. Extracts components of an \code{eemlist/eem} and \code{abslist/abs} object.
#'
#' @param x an object of class:
#' \itemize{
#' \item{\code{eemlist}}
#' \item{\code{eem}}
#' \item{\code{abslist}}
#' \item{\code{abs}}
#' }
#' @param info the name of the component within the \code{eem} or \code{abs} to extract.
#' see \link[eemR]{eem} for base \code{eem} component names and \link[eemanalyzeR]{abs_read} for base \code{abs} names.
#' see \link[eemanalyzeR]{add_metadata} for extended \code{eem} component names.
#'
#' @returns returns a vector containing the info from the EEM's or absorbance.
#'
#' @export
#'
#' @examples
#' #get names
#' get_sample_info(example_eems, "sample")
#' get_sample_info(example_absorbance, "sample")
#'
#' #get analysis_date
#' eemlist <- add_metadata(metadata, example_eems)
#' get_sample_info(eemlist, "analysis_date")
#'
#' #get doc for fifth eem
#' eemlist <- add_metadata(metadata, example_eems)
#' get_sample_info(eemlist[[5]], "doc_mgL")

get_sample_info <- function(x, info) {
  stopifnot(.is_eemlist(x) | .is_eem(x) | .is_abslist(x) | .is_abs(x))

  if(inherits(x, "eemlist") | inherits(x, "abslist") ){
    res <- unlist(lapply(x, function(y) y[[info]]))
  }
  if(inherits(x, "eem") | inherits(x, "abs")){
    res <- x[[info]]
  }
  return(res)
}

