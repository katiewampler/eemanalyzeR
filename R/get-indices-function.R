#' Get function to generate fluorescence and absorbance indices
#'
#' Accepts either a custom function to generate indices (see vignette) or uses preset methods.
#'
#' @param index_method currently supports "eemanalyzeR", "eemR", and "usgs" or a custom function
#'
#' @returns a function used to generate indices
#'
#' @export
#' @details
#' For more details on the preset methods see the following functions:
#' \itemize{
#' \item eemanalyzeR: \link[eemanalyzeR]{eemanalyzeR_indices}
#' \item eemR: \link[eemanalyzeR]{eemR_indices}
#' \item usgs: \link[eemanalyzeR]{usgs_indices}
#' }
#'
#'
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
