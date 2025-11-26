#' Get function to generate fluorescence and absorbance indices
#'
#' Returns either a user-supplied custom function or one of the preset index
#' generators.
#'
#' @param index_method Either "eemanalyzeR", "eemR", "usgs", or a custom function.
#'
#' @return A function used to generate indices.
#'
#' @details
#' Preset methods correspond to the following functions:
#'
#' - **eemanalyzeR**: [eemanalyzeR_indices()]
#' - **eemR**: [eemR_indices()]
#' - **usgs**: [usgs_indices()]
#'
#' @export
#' @md
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
