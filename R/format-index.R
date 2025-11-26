#' Format index and flag data
#'
#' Combines index values, QA/QC flags, and sample metadata into a clean
#' `data.frame` for further processing. Any values flagged as "MDL01"
#' (below detection limit) are removed and not reported.
#'
#' @param x An `eemlist` or `abslist` object.
#' @param index A character string giving the name of the index.
#' @param value Index values. Use [get_fluorescence()] or
#'   [get_absorbance()] to generate these values.
#' @param flag Flag values, the same length as `value`.
#'
#' @return A data frame with four columns:
#'
#' - **sample_name**: the sample name
#' - **meta_name**: the sample name in metadata (if provided),
#'   otherwise repeats `sample_name`
#' - **index**: the name of the index
#' - **value**: the index value (with `MDL01` values removed)
#'
#' @export
#' @md
#'
#' @examples
#' ex <- 240:260
#' em <- 300:320
#'
#' vals <- get_fluorescence(example_eems, ex, em, stat = "max")
#' flags <- flag_missing(example_eems, ex = ex, em = em, all = FALSE)
#'
#' index_formatted <- format_index(
#'   x = example_eems,
#'   index = "test_index",
#'   value = vals,
#'   flag = flags
#' )
format_index <- function(x, index, value, flag){
  stopifnot(length(value) == length(flag), .is_eemlist(x) | .is_abslist(x))

  #get sample names
    sample_name <- get_sample_info(x, "sample")
    #get meta name if metadata has been added, else get sample again
    if(all(.meta_added(x))){
      meta_name <- get_sample_info(x, "meta_name")
    }else{
      meta_name <-  get_sample_info(x, "sample")}

  #set values below MDL to NA
    value[grep("MDL01|MDL03", flag)] <- NA

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
