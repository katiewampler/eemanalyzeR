#' Extract QAQC samples from an eemlist or abslist
#'
#' Extracts either **instrument** blanks or tea check standards from an `eemlist` or `abslist`
#' using the samples attributes. Samples labeled using the the \link[eemanalyzeR]{mark_qaqc} function.
#' @md
#'
#' @param x an object of class `eemlist` or `abslist`
#' @param type either "blank" to return instrument blanks or "tea_std" to return tea check standard
#' @param negate logical, should the QAQC samples be returned or removed?

#' @returns an object of the same class as `x` with only the blanks or tea standards.
#' @export

#' @examples
#' #returns NULL because no samples are marked as QAQC
#' tea <- subset_qaqc(example_abs, "tea_std")
#' tea
#'
#' #get tea absorbance
#' abs <- mark_qaqc(example_abs, tea_pattern="Tea")
#' tea <- subset_qaqc(abs, "tea_std")
#' get_sample_info(tea, "sample")
#'
#' #get blank eems
#' eems <- mark_qaqc(example_eems, blk_pattern="BEM", tea_pattern="Tea")
#' blk <- subset_qaqc(eems)
#' get_sample_info(blk, "sample")
#'
#' #use negate = TRUE to get non qaqc samples
#' nonblk <- subset_qaqc(eems, negate=TRUE)
#' get_sample_info(nonblk, "sample")

subset_qaqc <- function(x, type=c("blank", "tea_std"), negate=FALSE){
  stopifnot(.is_eemlist(x) | .is_abslist(x))

  type <- match.arg(type)

  att <- ifelse(type == "blank", "is_blank", "is_check_std")

  index <- sapply(x, function(i){attr(i, att)})
  if(all(!index)){return(NULL)}

  if(negate){sub_x <- x[!index]}else{sub_x <- x[index]}

  class(sub_x) <- class(x)
  return(sub_x)
}
