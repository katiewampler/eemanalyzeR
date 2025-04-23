#' eemR and staRdom methods for fluorescence and absorbance indices
#'
#' Calculates commonly used absorbance and fluorescence optical indices from eemlist and abslist using
#' \href{https://CRAN.R-project.org/package=eemR}{eemR} and \href{https://CRAN.R-project.org/package=staRdom}{staRdom} functions.
#'
#' @importFrom eemR eem_coble_peaks eem_fluorescence_index eem_humification_index eem_biological_index
#' @importFrom staRdom abs_parms
#' @param eemlist an \code{eemlist} object containing EEM's data. See details for more info.
#' @param abslist an \code{abslist} object containing absorbance data.
#' @param cuvle cuvette (path) length in cm
#'
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
#' }
#'
#' @details
#' \strong{Absorbance Indices}: Absorbance indices (a254, a300, E2_E3, E4_E6, S275_295, S350_400, S300_700, SR)
#' are generated using the staRdom function \link[staRdom]{abs_parms}.
#'
#' \strong{Fluorescence Indices}: Fluorescence indices are generated using the eemR functions:
#' \itemize{
#' \item Coble peaks (b, t, a, m, c): \link[eemR]{eem_coble_peaks}
#' \item fi: \link[eemR]{eem_fluorescence_index}
#' \item hix: \link[eemR]{eem_humification_index}, where hix_scaled uses scale=TRUE
#' \item bix: \link[eemR]{eem_biological_index}

#' }
#'
#' @export
#'
#' @examples
#' abslist <- add_metadata(metadata, example_absorbance)
#' eemlist <- add_metadata(metadata, example_eems)
#' indices <- eemR_indices(eemlist, abslist)
eemR_indices <- function(eemlist, abslist, cuvle=1){
  stopifnot(.is_eemlist(eemlist), .is_abslist(abslist), is.numeric(cuvle), all(sapply(eemlist, attr, "is_doc_normalized"))==FALSE)

  #get fluoresence peaks
    #define wavelengths for peaks and metrics to check if there are missing wavelengths
    #format: index = list(excitation wavelengths, emission wavelengths, do all wavelengths need to exist to return value?)
    peaks <- list(b = list(ex=275, em=310, all=TRUE),
                  t = list(ex=275, em=340, all=TRUE),
                  a = list(ex=260, em=380:460, all=TRUE),
                  m = list(ex=312, em=380:420, all=TRUE),
                  c = list(ex=350, em=420:480, all=TRUE),
                  fi = list(ex=370, em=c(450, 500), all=TRUE),
                  hix = list(ex=254, em=c(300:345,435:480), all=TRUE),
                  hix_scaled = list(ex=254, em=c(300:345,435:480), all=TRUE),
                  bix = list(ex=310, em=c(380, 430), all=TRUE))

    #get metrics using eemR functions
    #get fluorescence indices
    eem_index <- do.call("rbind", lapply(eemlist, function(eem){
      suppressWarnings(vals <- c(eemR::eem_coble_peaks(eem)[-1],
                eemR::eem_fluorescence_index(eem)[2],
                eemR::eem_humification_index(eem, scale=FALSE)[2],
                hix_scaled = unname(eemR::eem_humification_index(eem, scale=TRUE)[2]),
                eemR::eem_biological_index(eem)[2]))
      name_type <- ifelse(.meta_added(eem), "meta_name", "sample")
      index <- data.frame(sample_name= get_sample_info(eem, "sample"),
                          meta_name=get_sample_info(eem, name_type),
                          index = names(vals),
                          value = unlist(unname(vals)))}))

    #add flags for missing data
    missing_data_eem <- function(peaks, eem){
      #get ranges of wavelengths in data
      ex_range <- min(eem$ex):max(eem$ex)
      em_range <- ceiling(min(eem$em)):floor(max(eem$em)) #round because they're usually not integers
      flags <- sapply(peaks, function(x){
        #is the range completely contained in ranges?
        if(all(x$ex %in% ex_range) & all(x$em %in% em_range)){
          return(NA)
        }else if(any(x$ex %in% ex_range) & any(x$em %in% em_range) & x$all == FALSE){
          return("DATA_02") #entire index range not contained in data
        }else{
          return("DATA_01") #index range not in data, unable to report value
        }
      })

      return(flags)


    }
    eem_flags <- do.call("rbind", lapply(eemlist, function(x){
      flags <- data.frame(sample_name= x$sample,
                          index = names(missing_data_eem(peaks,x)),
                          flag=missing_data_eem(peaks,x))}))
    eem_index <- plyr::join(eem_index, eem_flags, by=c("sample_name", "index"))

    #move flags to values (replace or add as needed)
    #DATA_01: missing data, unable to calculate index
    #DATA_02: missing some data, index may not be accurate
    eem_index$value[eem_index$flag == "DATA_01"] <- "DATA_01"
    partial <- eem_index$flag == "DATA_02"
    partial[is.na(partial)] <- FALSE
    eem_index$value[partial & is.na(eem_index$value)==F] <-
      paste0(eem_index$value[partial & is.na(eem_index$value)==F], "_DATA_02")

    eem_index <- eem_index %>% dplyr::select(-any_of("flag")) #remove flag column

  #absorbance peaks
    raw_abs <- get_sample_info(abslist, "data")
    abs_index <- staRdom::abs_parms(raw_abs, cuvle=1, unit = "absorbance", cores=1)
    abs_names <- do.call("rbind", lapply(abslist, function(abs){
      name_type <- ifelse(.meta_added(abs), "meta_name", "sample")
      index <- data.frame(sample_name= get_sample_info(abs, "sample"),
                          meta_name=get_sample_info(abs, name_type))
    }))
    abs_index <- cbind(abs_names, abs_index[,-1])

    #add flags for SR and slopes
    abs_index$S275_295[is.na(abs_index$S275_295)] <- "DATA_04"
    abs_index$S350_400[is.na(abs_index$S350_400)] <- "DATA_04"
    abs_index$SR[is.na(abs_index$SR)] <- "DATA_04"

    #make long
    abs_index[] <- lapply(abs_index, as.character)

    abs_index <- abs_index %>% tidyr::pivot_longer(cols = 3:10, names_to = "index", values_to = "value")


    #specify absorbance wavelengths to check if there's missing data
    #format: index = wavelengths in metric
    abs_wl <- list(a254 = 254, a300=300, E2_E3=c(250,365),
                   E4_E6=c(465,665),
                   S275_295 = 275:295, S350_400=350:400,
                   S300_700 = 300:700,
                   SR = c(275:295, 350:400))

    #add flags for missing wavelengths
    missing_data_abs <- function(peaks, abs){
      #get ranges of wavelengths in data
      range <- min(abs$data[,1]):max(abs$data[,1])

      flags <- sapply(peaks, function(x){
        #is the range completely contained in ranges?
        if(all(x %in% range)){
          return(NA)
        }else{
          return("DATA_01") #index range not in data, unable to report value
        }
      })

      return(flags)


    }
    abs_flags <- do.call("rbind", lapply(abslist, function(x){
      flags <- data.frame(sample_name= x$sample,
                          index = names(missing_data_abs(abs_wl,x)),
                          flag=missing_data_abs(abs_wl,x))}))
    abs_index <- plyr::join(abs_index, abs_flags, by=c("sample_name", "index"))



    #move flags to values (replace or add as needed)
    #DATA_01: missing data, unable to calculate index
    abs_index$value[abs_index$flag == "DATA_01"] <- "DATA_01"

    abs_index <- abs_index %>% dplyr::select(-any_of("flag")) #remove flag column

  #return indices
    index <- list(abs_index=abs_index, eem_index = eem_index)
    return(index)

}
