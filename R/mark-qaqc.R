
#' Specify QAQC samples
#'
#' Updates an `eemlist` or `abslist` with information about which samples are
#' **instrument** blanks or tea check standards. Marks `eem` or `abs` attributes `is_blank` and `is_check_std` as true.
#'
#' @param x an object of class `eemlist` or `abslist`
#' @param blk_pattern a character string containing a \code{\link[base]{regular expression}}
#' used to specify the sample names of the instrument blanks.
#' @param tea_pattern a character string containing a \code{\link[base]{regular expression}}
#' used to specify the sample names of the tea or check standard.
#' @param info the name of the component within the \code{eem} to check for the pattern. default is 'sample'
#'
#' @returns an object of the same class as `x`
#' @export
#'
#' @examples
#' eemlist <- mark_qaqc(example_eems, blk_pattern ="BEM", tea_pattern="Tea")
#' get_sample_info(eemlist[[1]], "sample")
#' attributes(eemlist[[1]])
#' get_sample_info(eemlist[[4]], "sample")
#' attributes(eemlist[[4]])
mark_qaqc <- function(x, blk_pattern=NULL, tea_pattern=NULL,info="sample"){
  type <- class(x)

  #mark blanks if pattern is provided
  if(!is.null(blk_pattern)){
    x <- lapply(x, function(i){
      is_blk <- grepl(blk_pattern, get_sample_info(i, info))
      if(is_blk){attr(i, "is_blank") <- TRUE}
      return(i)
    })
  }
  class(x) <- type

  #mark tea if pattern is provided
  if(!is.null(tea_pattern)){
    x <- lapply(x, function(i){
      is_tea <- grepl(tea_pattern, get_sample_info(i, info))
      if(is_tea){attr(i, "is_check_std") <- TRUE}
      return(i)
    })
  }
  class(x) <- type

  return(x)
}
