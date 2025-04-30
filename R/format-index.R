#' Format Index and Flag Data
#'
#' Takes index values, QA/QC flags, and dataset and combines and combines data and formats data for further processing.
#'
#' @param x an \code{eemlist} or \code{abslist} object
#' @param index a character, the name of the index
#' @param value the index values, use \link[eemanalyzeR]{get_fluorescence} or \link[eemanalyzeR]{get_absorbance} to get index values
#' @param flag the flag values, must be the same length as value, use \link[eemanalyzeR]{flag_missing} to generate flags
#'
#' @returns a data.frame with four columns:
#' #' \itemize{
#'  \item sample_name: the name of the sample
#'  \item meta_name: the name of the sample in the metadata if metadata has been added, otherwise the sample name again
#'  \item index: the name of the index being reported
#'  \item value: the value of the index
#' }
#' @export
#'
#' @examples
#'  ex <- 240:260
#'  em <- 300:320
#'  vals <- get_fluorescence(example_eems, ex, em, stat = "max")
#'  flags <- flag_missing(example_eems, ex=ex, em=em, all=FALSE)
#'  index_formatted <- format_index(example_eems, "test_index", vals, flags)

format_index <- function(x, index, value, flag){
  stopifnot(length(value) == length(flag), .is_eemlist(x) | .is_abslist(x))

  #get sample names
    sample_name <- get_sample_info(x, "sample")
    #get meta name if metadata has been added, else get sample again
    if(all(.meta_added(x))){
      meta_name <- get_sample_info(x, "meta_name")
    }else{
      meta_name <-  get_sample_info(x, "sample")}


  #combine vals and flags
    merge <- !is.na(value) & !is.na(flag) & value != flag
    value[is.na(value)] <- flag[is.na(value)]
    value[merge] <- paste0(value[merge], "_", flag[merge])

  #format as data.frame
    res <- data.frame(sample_name=sample_name,
                      meta_name=meta_name,
                      index=index,
                      value=unname(value))

  return(res)
}
