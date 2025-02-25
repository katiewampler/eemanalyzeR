#' Extract tea from abslist
#'
#' Using a regular expression, the \code{abslist} will be cut to include either
#' only the tea samples (\code{abs_get_tea}).
#'
#' @param abslist an object of class \code{abslist}
#' @param pattern a character string containing a \code{\link[base]{regular expression}}
#' used to specify the sample names of the blanks.
#' @param info the name of the component within the \code{abs} to check for the pattern. default is 'sample'

#' @note see \link[eemR]{eem} for base \code{eem} component names and \link[eemanalyzeR]{add_metadata}
#' for extended \code{eem} component names.
#'
#' @returns an object of class \code{abslist} with only the tea standards.
#' @export
#'
#' @rdname extract_tea
#
#' @examples
#' tea <- abs_get_tea(example_absorbance, pattern = "tea")
abs_get_tea <- function(abslist, pattern = "tea", info = "sample") {

  tea_names <- grep(pattern, get_sample_info(abslist, info), value = T, ignore.case = TRUE)
  if(length(tea_names) > 0){
    abslist <- subset_samples(abslist, info, tea_names, keep = TRUE, ignore_case = TRUE, verbose = FALSE)
  }else{
    abslist <- list()
  }
  class(abslist) <- "abslist"
  return(abslist)
}
