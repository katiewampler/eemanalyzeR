#' Normalize an eem or eemlist based on a normalization factor
#'
#' Useful for raman normalization or normalizing to a max of one for blank comparisons.
#'
#' @param eem the \code{eem} or \code{eemlist} to normalize
#' @param factor the normalization factor, either a single value or vector of factors, if NULL it will normalize to the maximum value for each eem
#'
#' @return an \code{eem} or \code{eemlist} where \code{x} has been normalized
#' @export
#'
#' @examples
#' eem_normal <- eem_normalize(example_eems[1])
#' eems_normal <- eem_normalize(example_eems)
#'
eem_normalize <- function(eem, factor=NULL){

  if(.is_eemlist(eem)){
    eem <- mapply(eem_normalize, eem, factor, SIMPLIFY = F)
    class(eem) <- "eemlist"
  }else{
    if(is.null(factor)){
      factor <- max(eem$x, na.rm = T)
    }

    eem$x <- eem$x / factor
    class(eem) <- "eem"
  }

  return(eem)
}
