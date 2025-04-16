# TODO add description of what this does, add details linking to the sources of the different
# index functions

#' Get preset or use custom functions to generate fluorescence and absorbance indices
#'
#' @param index_method currently supports "eemanalyzeR", "eemR", and "usgs". See details for more information.
#'
#' @returns a function used to generate indices
#' @export
#'
get_indices_function <- function(index_method="eemanalyzeR"){

  if(is.function(index_method)){
    return(index_method)
  }

  switch(index_method,
         "eemanalyzeR" = eemanalyzeR_indices,
         "eemR" = eemR_indices,
         "usgs" = usgs_indices,
         stop(index_method, " is not a known function to generate indices\n  to create your own see vingette browseVingettes('eemanalyzeR')"))
}





eemR_indices <- function(){
  print("eemR indices")
}

usgs_indices <- function(){
  print("usgs indices")
}

#' Get fluoresence and absorbance indices
#'
#' @param eemlist an \code{eemlist} object containing EEM's data. See details for more info.
#' @param abslist an \code{abslist} object containing absorbance data.
#' @param index_method currently supports "eemanalyzeR", "eemR", and "usgs". See \link[eemanalyzeR]{get_indices_function} for more information.

#' @return a data.frame with index values for each sample with a row for each sample
#'
get_indices <- function(eemlist, abslist, index_method){
  #TODO: warn if processing steps haven't been done


  #get function to get indices
  index_function <- get_indices_function(index_method)

  #get indices

  #flag if needed, put flags in place of value, mark NA values with why, if want a value do val_flag
    #check for ratios below noise, missing values due to no DOC or outside measurement range

  #return


}
