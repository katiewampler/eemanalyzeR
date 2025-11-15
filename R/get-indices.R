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
#' @param mdl_dir file path to the mdl files generated with \link[eemanalyzeR]{create_mdl} and \link[eemanalyzeR]{create_tea_std},
#' default is a user-specific data directory (\link[rappdirs]{user_data_dir})
#' @param arg_names optional argument used to pass arguments from higher level functions for writing the readme.
#'
#' @export
#' @return a list with two objects:
#' \itemize{
#'   \item eem_index: a data.frame of all the fluorescence indices
#'   \item abs_index: a data.frame of all the absorbance indices
#' }
#' If \code{return} is 'long' each data.frame will have four columns:
#' \itemize{
#'  \item sample_name: the name of the sample
#'  \item meta_name: the name of the sample in the metadata if metadata has been added, otherwise the sample name again
#'  \item index: the name of the index being reported, see details for more information.
#'  \item value: the value of the index
#'  \item QAQC_flag: any flags associated with the sample, see details for more information.
#' }
#'
#' If \code{return} is 'wide' each data.frame will have a row for each sample, where the columns are the different indices. In this format QAQC flags
#' are indicated in the individuals cells, either by itself if no value was able to be reported, or combined with the value where applicable to have the form
#' 'value_flag'.
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
#'  \item DATA01: Missing data required to calculate the index
#'  \item DATA02: Missing some wavelengths required to calculate the index, value may be inaccurate
#'  \item DATA03: Unable to calculate ratio because denominator was zero
#'  \item DATA04: Spectral slope was unable to be calculated
#'  \item DOC01: Missing dissolved organic carbon data, so index was not able to be calculated
#'  \item MDL01: All values were below the MDL for those wavelengths
#'  \item MDL02: One of more of the values were below the MDL for those wavelengths, use data cautiously
#'  \item MDL03: Index was a ratio, and either numerator or denominator was completely below the MDL
#'  \item NEG01: Value was negative
#'  \item NOISE01: Value was below signal to noise ratio and therefore was not calculated
#'  \item NOISE02: Value was not able to be checked for signal to noise ratio
#'  \item VAL01: Value was below expected values for this index, normal ranges can vary by sample matrix, but please check value for accuracy
#'  \item VAL02: Value was above expected values for this index, normal ranges can vary by sample matrix, but please check value for accuracy
#' }
#' @examples
#' indices <- get_indices(example_processed_eems, example_processed_abs,
#' mdl_dir = system.file("extdata", package = "eemanalyzeR"))

get_indices <- function(eemlist, abslist, index_method="eemanalyzeR", return ="long",
                        cuvle=1, mdl_dir= .qaqc_dir(), arg_names=NULL){
  stopifnot(.is_eemlist(eemlist), .is_abslist(abslist))

  #check if processing has been done, not warn that indices may be unreliable
    steps <- check_processing(eemlist)
    steps <- steps[-nrow(steps),] #remove check for DOC

    if(all(!steps$done)){
      warning("Data has not been processed, and indices may not be accurate. \nPlease use eemanalyzeR::process_eem to process EEMs before generating indices.")
    }else if(any(!steps$done)){
      missing <- steps[steps$done == FALSE,]
      warning("Data has not been fully processed, and indices may not be accurate. The following processing steps are missing:\n",
              paste(missing$warning, collapse="\n"), "\n\nPlease use eemanalyzeR::process_eem to process EEMs before generating indices.")}

  #collect arguments for readme, and to put into the following functions
    if(is.null(arg_names)){
      args <- rlang::enquos(index_method, return, cuvle, mdl_dir)
      names(args) <- c("index_method", "return", "cuvle", "mdl_dir")
    }else{args <- arg_names}

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
  indices <- index_function(eemlist, abslist, cuvle = cuvle, mdl_dir=mdl_dir)

  #flag if needed
    #helper functions to make flags
      missing_doc_flag <- function(index){
        #if index is NA, return NA
        if(!is.data.frame(index)){return(index)}
        doc_flag <- grepl("SUVA|SVA|DOC", index$index) & is.na(index$value) & is.na(index$QAQC_flag)
        index$QAQC_flag[doc_flag] <- "DOC01"
        return(index)
      }
      negative_flag <- function(index){
        #if index is NA, return NA
        if(!is.data.frame(index)){return(index)}
        negative <- index$value < 0
        negative[is.na(negative)] <- FALSE
        index$QAQC_flag[negative] <- "NEG01"
        index$value[negative] <- NA
        return(index)
      }
      move_flags <- function(index){
        #if index is NA, return NA
        if(!is.data.frame(index)){return(index)}

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
        #if index is NA, return NA
        if(!is.data.frame(index)){return(index)}
        ranges <- eemanalyzeR::indice_ranges[eemanalyzeR::indice_ranges$index_method == index_method,]

        index <- plyr::join(index, ranges, by="index")
        low <- as.numeric(index$value) < index$low_val
        low[is.na(low)] <- FALSE
        index$QAQC_flag[low] <- "VAL01"

        high <- as.numeric(index$value) > index$high_val
        high[is.na(high)] <- FALSE
        index$QAQC_flag[high] <- "VAL02"

        index <- index %>% dplyr::select(-any_of(c("low_val", "high_val", "sources", "index_method")))

        return(index)
      }
      infinite_flag <- function(index){
        #if index is NA, return NA
        if(!is.data.frame(index)){return(index)}
        infinite <- index$value == Inf
        infinite[is.na(infinite)] <- FALSE
        index$QAQC_flag[infinite] <- "DATA03"
        index$value[infinite] <- NA
        return(index)
      }

    #inf values
    indices <- lapply(indices, infinite_flag)

    #missing data (no wavelengths)
      indices <- lapply(indices, move_flags)

    #negative values
      indices <- lapply(indices, negative_flag)

    #missing data (no DOC): DOC_01
      indices <- lapply(indices, missing_doc_flag)

    #questionable, outside normal range
      if(is.character(index_method)){
        indices <- lapply(indices, outside_range)

      }

    #check tea standards
      std_check <- check_tea_std(eemlist, abslist, std_dir=mdl_dir)

  #make indices numeric
   indices <- lapply(indices, function(x){
     #if index is NA, return NA
     if(!is.data.frame(x)){return(x)}
     x$value <- as.numeric(x$value)
     return(x)})

  #change missing NA values to -9999 to indicate they're missing on purpose
    indices <- lapply(indices, function(x){
      #if index is NA, return NA
      if(!is.data.frame(x)){return(x)}
      x$value[is.na(x$value)] <- -9999
    return(x)})

    indices <- lapply(indices, function(x){
      #if index is NA, return NA
      if(!is.data.frame(x)){return(x)}
      x$QAQC_flag[is.na(x$QAQC_flag)] <- "N/A"
      return(x)})

  #return
    if(return == "wide"){
      indices <- lapply(indices, function(x){
        #if index is NA, return NA
        if(!is.data.frame(x)){return(x)}
        x$value_flag <- NA
        x$value_flag[x$value == -9999] <- x$QAQC_flag[x$value == -9999]
        x$value_flag[x$QAQC_flag == "N/A"] <- signif(x$value[x$QAQC_flag == "N/A"], 4)
        x$value_flag[x$value != -9999 & x$QAQC_flag != "N/A"] <- paste(signif(x$value[x$value != -9999 & x$QAQC_flag != "N/A"], 4),
                                                                               x$QAQC_flag[x$value != -9999 & x$QAQC_flag != "N/A"], sep="_")

        x_wide <- x %>% dplyr::select(-any_of(c("value", "QAQC_flag"))) %>% tidyr::pivot_wider(names_from="index", values_from="value_flag")
        return(x_wide)

      })
    }

  #write step to readme
  if(is.character(index_method)){
   .write_readme_line("Absorbance and fluorescence indices were calculated using the 'get_indices' function", "indices", args)
  }else{
    .write_readme_line("Absorbance and fluorescence indices were calculated using the 'get_indices' function with a custom index method", "indices", args)

  }
    return(indices)
}
