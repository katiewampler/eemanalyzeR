#' Extract Samples from an eemlist or abslist by sample type
#'
#' Extracts the specified sample type(s) from an `eemlist` or `abslist`
#' using the samples attribute `sample_type`. Samples labeled using the the \link[eemanalyzeR]{add_metadata} function.
#' @md
#'
#' @param x an object of class `eemlist` or `abslist`
#' @param type either 'iblank' for instrument blank, 'sblank' for analytical blank, 'check' for tea check standard, or 'sample' for samples
#' @param negate logical, should the samples be returned or removed?

#' @returns an object of the same class as `x`.
#' @export

#' @examples
#' abs <- add_metadata(metadata, example_abs)
#' eem <- add_metadata(metadata, example_eems)
#'
#' #returns NULL because no samples are marked as instrument blanks
#' tea <- subset_type(abs, "iblank")
#' tea
#'
#' #get tea absorbance
#' tea <- subset_type(abs, "sblank")
#' get_sample_info(tea, "sample")
#'
#' #get blank eems (instrument and analytical)
#' blk <- subset_type(eem, c("iblank", "sblank"))
#' get_sample_info(blk, "sample")
#'
#' #use negate = TRUE to get non instrument blanks
#' nonblk <- subset_type(eem, "iblank", negate=TRUE)
#' get_sample_info(nonblk, "sample")

subset_type <- function(x, type=c("iblank", "sblank", "check", "sample"), negate=FALSE){
  stopifnot(.is_eemlist(x) | .is_abslist(x))

  type <- match.arg(type, several.ok = TRUE)

  x_type <- sapply(x, attr, "sample_type")

  index <- x_type %in% type

  if(all(!index)){return(NULL)}

  if(negate){sub_x <- x[!index]}else{sub_x <- x[index]}

  return(sub_x)
}
