# TODO add details linking to the sources of the different
# index functions

#' Get preset or use custom functions to generate fluorescence and absorbance indices
#'
#' @param index_method currently supports "eemanalyzeR", "eemR", and "usgs". See details for more information.
#'
#' @returns a function used to generate indices
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

#' Get fluorescence and absorbance indices
#'
#' Calculates commonly use indices from absorbance and excitation emission matrix (EEM) data. Also
#' checks and flags the indices for potential errors or issues.
#'
#' @param eemlist an \code{eemlist} object containing EEM's data.
#' @param abslist an \code{abslist} object containing absorbance data.
#' @param index_method currently supports "eemanalyzeR", "eemR", and "usgs". See details for more information.
#' @param return either "long" or "wide" to specify the format of the indices data.frames
#' @param cuvle cuvette (path) length in cm
#'
#' @export
#' @return a list with two objects:
#' \itemize{
#'   \item eem_index: a data.frame of all the fluorescence indices
#'   \item abs_index: a data.frame of all the absorbance indices
#' }
#' Each data.frame will have four columns:
#' \itemize{
#'  \item sample_name: the name of the sample
#'  \item meta_name: the name of the sample in the metadata if metadata has been added, otherwise the sample name again
#'  \item index: the name of the index being reported, see details for more information.
#'  \item value: the value of the index
#'  \item QAQC_flag: any flags associated with the sample, see details for more information.
#' }
#'
#' @details
#' \strong{Index Methods}
#'
#' This function allows for three different sets of indices:
#' \href{../doc/eemanalyzeR-indices.html}{\code{eemanalyzeR}} (default),
#' eemR and usgs. See the links for more information on the indices in each method and how they're calculated.
#' Custom functions can also be used to calculate indices, see vignette for more information on how to do this.
#TODO: link vignette here
#'
#'\strong{QA/QC Flags}
#'
#' The index values will be checked for potential errors prior to reporting and flagged if necessary:
#' \itemize{
#'  \item DATA_01: Missing data required to calculate the index
#'  \item DATA_02: Missing some wavelengths required to calculate the index, value may be inaccurate
#'  \item DATA_03: Unable to calculate ratio because denominator was zero
#'  \item DATA_04: Spectral slope was unable to be calculated
#'  \item DOC_01: Missing dissolved organic carbon data, so index was not able to be calculated
#'  \item NEG_01: Value was negative
#'  \item NOISE_01: Value was below signal to noise ratio and therefore was not calculated
#'  \item NOISE_02: Value was below signal to noise ratio and may be inaccurate
#'  \item VAL_01: Value was below expected values for this index, please check for accuracy
#'  \item VAL_02: Value was above expected values for this index, please check for accuracy
#' }
#'
#' @examples
#' abslist <- add_metadata(metadata, example_absorbance)
#' eemlist <- add_metadata(metadata, example_eems)
#' indices <- get_indices(eemlist, abslist)

get_indices <- function(eemlist, abslist, index_method="eemanalyzeR", return ="long", cuvle=1){
  stopifnot(.is_eemlist(eemlist), .is_abslist(abslist))

  #check if processing has been done, not warn that indices may be unreliable
    steps <- check_processing(eemlist)
    steps <- steps[-nrow(steps),] #remove check for DOC

    if(!(all(steps$done))){
      warning("Data has not been processed, and indices may not be accurate. \nPlease use eemanalyzeR::process_eem to process EEMs before generating indices.")
    }else if(!any(steps$done)){
      missing <- steps[steps$done == FALSE,]
      warning("Data has not been fully processed, and indices may not be accurate. The following processing steps are missing:\n",
              paste(missing$warning, collapse="\n"), "\n\nPlease use eemanalyzeR::process_eem to process EEMs before generating indices.")}

  #if DOC normalized, make not normalized to not normalize twice for indices
    eemlist <- lapply(eemlist, function(x){
      if(attr(x, "is_doc_normalized")){
        x$x <- x$x * x$doc_mgL #multiply by doc to get un-normalized EEM's values
        attr(x, "is_doc_normalized") <- FALSE
      }else{x}
      return(x)
    })
    class(eemlist) <- "eemlist"

  #get function to get indices
  index_function <- get_indices_function(index_method)

  #get indices
  indices <- index_function(eemlist, abslist, cuvle = cuvle)

  #flag if needed
    #helper functions to make flags
      missing_doc_flag <- function(index){
        doc_flag <- grepl("SUVA|SVA|DOC", index$index) & is.na(index$value) & is.na(index$QAQC_flag)
        index$QAQC_flag[doc_flag] <- "DOC_01"
        return(index)
      }
      negative_flag <- function(index){
        negative <- index$value < 0
        negative[is.na(negative)] <- FALSE
        index$QAQC_flag[negative] <- "NEG_01"
        index$value[negative] <- NA
        return(index)
      }
      move_flags <- function(index){
        #move flags from value column to QA/QC column and replace with NA or value
          #check if there's a flag (not numeric)
            flagged <- !grepl("^[-+]?\\d*(\\.\\d+)?([eE][-+]?\\d+)?$", index$value) & !is.na(index$value)

          if(any(flagged)){
            #move flag values and replace with NA
            index$QAQC_flag[flagged] <- index$value[flagged]
            index$value[flagged] <- NA

            #move provisional values back to value
            prov <- grepl("^\\d*\\.?\\d+_[^_]+_\\d+$", index$QAQC_flag)
            index$value[prov] <- stringr::str_split_i(index$QAQC_flag[prov], "_", i=1)
            index$QAQC_flag[prov] <- paste(stringr::str_split_i(index$QAQC_flag[prov], "_", i=2),
                                        stringr::str_split_i(index$QAQC_flag[prov], "_", i=3), sep="_")

          }

          return(index)
      }
      outside_range <- function(index){
        index <- plyr::join(index, eemanalyzeR::indice_ranges, by="index")
        low <- as.numeric(index$value) < index$low_val
        low[is.na(low)] <- FALSE
        index$QAQC_flag[low] <- "VAL_01"

        high <- as.numeric(index$value) > index$high_val
        high[is.na(high)] <- FALSE
        index$QAQC_flag[high] <- "VAL_02"

        index <- index %>% dplyr::select(-any_of(c("low_val", "high_val", "sources")))

        return(index)
      }


    #missing data (no wavelengths)
      indices <- lapply(indices, move_flags)

    #if ratios or values are below noise

    #negative values
      indices <- lapply(indices, negative_flag)

    #missing data (no DOC): DOC_01
      indices <- lapply(indices, missing_doc_flag)

    #questionable, outside normal range
      indices <- lapply(indices, outside_range)

  #make indices numeric
   indices <- lapply(indices, function(x){
     x$value <- as.numeric(x$value)
     return(x)})

  #change missing NA values to -9999 to indicate they're missing on purpose
    indices <- lapply(indices, function(x){
      x$value[is.na(x$value)] <- -9999
    return(x)})

  #return
    if(return == "wide"){
      #TODO: will need to figure out combining flag strings later
      abs_index <- tidyr::pivot_wider(abs_index, names_from="metric", values_from="value")
      eem_index <- tidyr::pivot_wider(abs_index, names_from="metric", values_from="value")

    }

  #write step to readme
  if(is.character(index_method)){
   .write_readme_line("Absorbance and fluorescence indices were calculated using the 'get_indices' function")
  }else{
    .write_readme_line("Absorbance and fluorescence indices were calculated using the 'get_indices' function with a custom index method")

  }
    return(indices)
}
