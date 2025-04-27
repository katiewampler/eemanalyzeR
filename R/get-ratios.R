#' Safely return ratios values
#'
#' Will either return a ratio value or a QA/QC flag if ratio value can't be calculated.
#'
#' The index values will be checked for potential errors prior to reporting and flagged if necessary:
#' \itemize{
#'  \item DATA_01: Missing data required to calculate the index
#'  \item DATA_03: Unable to calculate ratio because denominator was zero}
#'
#' @param val1 the numerator of the ratio, can be a single value or a vector of values
#' @param val2 the denominator of the ratio, can be a single value or a vector of values
#'
#' @return a ratio value if possible or a character flag
#' @export
#'
#' @examples
#' #calculate the ratio of Peak A to Peak T
#' pA <- get_fluorescence(example_eems, ex=250:260, em=380:480)
#' pT <- get_fluorescence(example_eems, ex=270:280, em=320:350)
#' rAT <- get_ratios(pA, pT)
#'
get_ratios <- function(val1, val2){
  stopifnot(length(val1) == length(val2) | length(val1) ==1 | length(val2)==1)

  #if only one val given, make length of other
  if(length(val1) == 1){val1 <- rep(val1, length(val2))}
  if(length(val2) == 1){val2 <- rep(val2, length(val1))}

  vals <- c()
  for(x in 1:length(val1)){
    if(is.na(val1[x]) | is.na(val2[x])){
      val <- "DATA_01"
    }else if(val2[x] == 0){
      val <- "DATA_03"
    }else{
      val <- unname(val1[x] / val2[x])
    }
    vals <- c(vals, val)
  }

  return(vals)
}
