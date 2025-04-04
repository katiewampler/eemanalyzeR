# TODO add description of what this does, add details linking to the sources of the different
# index functions

#' Get preset or use custom functions to generate fluorescence and absorbance indices
#'
#' @param method currently supports "eemanalyzeR", "eemR", and "usgs". See details for more information.
#'
#' @returns a function used to generate indices
#' @export
#'
get_indices_function <- function(method="eemanalyzeR"){

  if(is.function(method)){
    return(method)
  }

  switch(method,
         "eemanalyzeR" = eemanalyzeR_indices,
         "eemR" = eemR_indices,
         "usgs" = usgs_indices,
         stop(method, " is not a known function to generate indices\n  to create your own see vingette browseVingettes('eemanalyzeR')"))
}

eemanalyzeR_indices <- function(){
  print("eemanalyzeR indices")
}

eemR_indices <- function(){
  print("eemR indices")
}

usgs_indices <- function(){
  print("usgs indices")
}
