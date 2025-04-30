#' Default package methods for fluorescence and absorbance indices
#'
#' Calculates commonly used absorbance and fluorescence optical indices from eemlist and abslist.
#' For detailed descriptions and references for indices, see the vignette
#' \href{../doc/eemanalyzeR-indices.html}{\code{eemanalyzeR-indices}}.
#' @importFrom zoo na.approx
#' @importFrom tidyr pivot_wider
#' @importFrom pracma interp2
#'
#' @param eemlist an \code{eemlist} object containing EEM's data. See details for more info.
#' @param abslist an \code{abslist} object containing absorbance data.
#' @param cuvle cuvette (path) length in cm
#' @note If absorbance is not at a 1 nanometer interval, absorbance will be interpolated using
#' \link[zoo]{na.approx} which fills in missing values
#' using linear interpolation.
#'
#' If EEMs data is not a 1 nanometer interval, fluorescence will be interpolated
#' using \link[pracma]{interp2}.
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
#'  \item index: the name of the index being reported, see \href{../doc/eemanalyzeR-indices.html}{vignette}for more information.
#'  \item value: the value of the index
#' }
#'
#' @export
#'
#' @examples
#' abslist <- add_metadata(metadata, example_absorbance)
#' eemlist <- add_metadata(metadata, example_eems)
#' indices <- eemanalyzeR_indices(eemlist, abslist)
eemanalyzeR_indices <- function(eemlist, abslist, cuvle=1){
  stopifnot(.is_eemlist(eemlist), .is_abslist(abslist), is.numeric(cuvle), all(sapply(eemlist, attr, "is_doc_normalized"))==FALSE)

  #define wavelengths for peaks and metrics to check if there are missing wavelengths
    #format: index = list(excitation wavelengths, emission wavelengths)
  peaks <- list(pB = list(ex=270:280, em=300:320),
                pT = list(ex=270:280, em=320:350),
                pA = list(ex=250:260, em=380:480),
                pM = list(ex=310:320, em=380:420),
                pC = list(ex=330:350, em=420:480),
                pD = list(ex=390, em=509),
                pE = list(ex=455, em=521),
                pN = list(ex=280, em=370),
                rAT = list(ex=c(250:260,270:280), em=c(380:480,320:350)),
                rCA = list(ex=c(250:260,330:350), em=c(380:480,420:480)),
                rCM = list(ex=c(330:350,310:320), em=c(420:480,380:420)),
                rCT = list(ex=c(330:350,270:280), em=c(420:480,320:350)),
                pB_DOCnorm = list(ex=270:280, em=300:320),
                pT_DOCnorm = list(ex=270:280, em=320:350),
                pA_DOCnorm = list(ex=250:260, em=380:480),
                pM_DOCnorm = list(ex=310:320, em=380:420),
                pC_DOCnorm = list(ex=330:350, em=420:480),
                pD_DOCnorm = list(ex=390, em=509),
                pE_DOCnorm = list(ex=455, em=521),
                pN_DOCnorm = list(ex=280, em=370),
                FI = list(ex=370, em=c(470, 520)),
                HIX = list(ex=254, em=c(300:345,435:480)),
                HIX_ohno = list(ex=254, em=c(300:345,435:480)),
                fresh = list(ex=310, em=c(380, 420:435)),
                BIX = list(ex=310, em=c(380, 430)))

    #Get Coble Peaks and Ratios
      coble <- lapply(names(peaks[1:8]), function(index_name){
        index <- peaks[[index_name]]

        #get values
        vals <- get_fluorescence(eemlist, index$ex, index$em, stat = "max")

        #get flags
        flags <- flag_missing(eemlist, ex=index$ex, em=index$em, all=FALSE)

        #add sample names and make into data.frame (get index name)
        res <- format_index(eemlist, index_name, vals, flags)

        #return res
        return(res)
      })
      coble <- do.call(rbind, coble)

      ratio_map <- list(rAT = c("pA", "pT"),
                        rCA = c("pC", "pA"),
                        rCM = c("pC", "pM"),
                        rCT = c("pC", "pT"))
      coble_ratios <- lapply(names(peaks[9:12]), function(index_name){
        index <- peaks[[index_name]]

        #get values
        ratio_keys <- ratio_map[[index_name]]
        numerator <- coble$value[coble$index == ratio_keys[1]]
        denominator <- coble$value[coble$index == ratio_keys[2]]

        #remove flags if needed
        numerator <- stringr::str_split_i(numerator, "_", i=1)
        denominator <- stringr::str_split_i(denominator, "_", i=1)

        #if flag is a DATA_01, will get "DATA" replace with NA
        numerator[numerator == "DATA"] <- NA
        denominator[denominator == "DATA"] <- NA

        #get values
        vals <- get_ratios(numerator, denominator)

        #get flags
        flags <- flag_missing(eemlist, ex=index$ex, em=index$em, all=FALSE)

        #add sample names and make into data.frame (get index name)
        res <- format_index(eemlist, index_name, vals, flags)

        #return res
        return(res)
      })
      coble_ratios <- do.call(rbind, coble_ratios)

      coble_norm <- lapply(names(peaks[13:20]), function(index_name){
        index <- peaks[[index_name]]

        #get values
        vals <- get_fluorescence(eemlist, index$ex, index$em, stat = "max", norm=TRUE)

        #get flags
        flags <- flag_missing(eemlist, ex=index$ex, em=index$em, all=FALSE)

        #add sample names and make into data.frame (get index name)
        res <- format_index(eemlist, index_name, vals, flags)

        #return res
        return(res)
      })
      coble_norm <- do.call(rbind, coble_norm)

    #get FI
      index <- "FI"
      vals <- get_ratios(get_fluorescence(eemlist, 370, 470), get_fluorescence(eemlist, 370, 520))
      flags <- flag_missing(eemlist, ex=peaks[[index]]$ex, em=peaks[[index]]$ex, all=FALSE)
      FI <- format_index(eemlist, index, vals, flags)

    #get HIX
      index <- "HIX"
      low <- get_fluorescence(eemlist, 254, 300:345, stat="sum")
      high <- get_fluorescence(eemlist, 254, 435:480, stat="sum")
      vals <- get_ratios(high, low)
      flags <- flag_missing(eemlist, ex=peaks[[index]]$ex, em=peaks[[index]]$ex, all=FALSE)
      HIX <- format_index(eemlist, index, vals, flags)

      index <- "HIX_ohno"
      low_high <- get_fluorescence(eemlist, 254, c(300:345, 435:480), stat="sum")
      vals <- get_ratios(high, low_high)
      flags <- flag_missing(eemlist, ex=peaks[[index]]$ex, em=peaks[[index]]$ex, all=FALSE)
      HIX_ohno <- format_index(eemlist, index, vals, flags)

    #get freshness index
      index <- "fresh"
      vals <- get_ratios(get_fluorescence(eemlist, 310, 380), get_fluorescence(eemlist, 310, 420:435, stat="max"))
      flags <- flag_missing(eemlist, ex=peaks[[index]]$ex, em=peaks[[index]]$ex, all=FALSE)
      fresh <- format_index(eemlist, index, vals, flags)

    #get BIX
      index <- "BIX"
      vals <- get_ratios(get_fluorescence(eemlist, 310, 380), get_fluorescence(eemlist, 310, 430))
      flags <- flag_missing(eemlist, ex=peaks[[index]]$ex, em=peaks[[index]]$ex, all=FALSE)
      BIX <- format_index(eemlist, index, vals, flags)


  #merge indices together
     eem_index <- do.call(rbind, list(coble, coble_ratios, coble_norm, FI, HIX, HIX_ohno, fresh, BIX ))

  #absorbance peaks
    #interpolate absorbance data to 1 nm intervals
    abs_interp <- lapply(abslist, function(x){
      abs <- data.frame(x$data)
      abs_filled <- merge(abs, data.frame(X1=min(abs$X1):max(abs$X1)), all=T)
      abs_filled <- zoo::na.approx(abs_filled)
      x$data <- as.matrix(abs_filled)
      x$n <- nrow(abs_filled)
      return(x)
    })

    #specify absorbance wavelengths to check if there's missing data
      #format: index = wavelengths in metric
    abs_wl <- list(SUVA254 = 254, SUVA280=280, SVA412 = 412,
                    S275_295 = 275:295, S350_400=350:400,
                    SR = c(275:295, 350:400), E2_E3=c(250,365),
                    E4_E6=c(465,665))

    #helper functions to get absorbance indices
    calc_suva <- function(abs){
      suva_metrics <- sapply(abs_wl[1:3], function(wl){
        if(.meta_added(abs)){
          #calc suva values, absorbance/ DOC * 100 (to correct for cuvette length and get in m)
          abs_val <- unname(abs$data[abs$data[,1] == wl,2]) /abs$doc_mgL *  (100/cuvle)
        }else{
          abs_val <- NA
        }})
      return(suva_metrics)}

    #DATA_04: Spectral slope was unable to be calculated
    calc_ratios <- function(abs){
      #get absorption in m^-1 (convert from absorbance to absorption based on Beer's Law)
      absorption <- abs$data[,2] * log(10) / (cuvle/100) #convert cm cuvette to m

      S275_295 <- staRdom::abs_fit_slope(abs$data[,1], absorption,
                                     lim=c(275,295), l_ref=275)$coefficient
      S350_400 <- staRdom::abs_fit_slope(abs$data[,1], absorption,
                                     lim=c(350,400), l_ref=350)$coefficient

      #prevent non numeric values
      if(!is.numeric(S275_295)){S275_295 <- "DATA04"}
      if(!is.numeric(S350_400)){S350_400 <- "DATA04"}

      if(S275_295 == "DATA_04" |S350_400 == "DATA04" ){
       SR <- "DATA04"}else{
         SR <- get_ratios(S275_295, S350_400)
       }

      E2 <-  abs$data[abs$data[,1]==250,2]
      E3 <- abs$data[abs$data[,1]==365,2]
      E2_E3 <- get_ratios(E2, E3)

      E4 <-  abs$data[abs$data[,1]==465,2]
      E6 <- abs$data[abs$data[,1]==665,2]
      E4_E6 <- get_ratios(E4, E6)

      vals <- unname(c(S275_295, S350_400, SR, E2_E3, E4_E6)) #remove previous names
      names(vals) <- c("S275_295", "S350_400", "SR", "E2_E3", "E4_E6")

      return(vals)
  }

    #get all indices
    abs_index <- do.call("rbind", lapply(abs_interp, function(abs){
      vals <- c(calc_suva(abs), calc_ratios(abs))
      name_type <- ifelse(.meta_added(abs), "meta_name", "sample")
      index <- data.frame(sample_name= get_sample_info(abs, "sample"),
                          meta_name=get_sample_info(abs, name_type),
                          index = names(vals),
                          value = unname(vals))}))

    #add flags for missing wavelengths
    missing_data_abs <- function(peaks, abs){
      #get ranges of wavelengths in data
      range <- min(abs$data[,1]):max(abs$data[,1])

      flags <- sapply(peaks, function(x){
        #is the range completely contained in ranges?
        if(all(x %in% range)){
          return(NA)
        }else{
          return("DATA01") #index range not in data, unable to report value
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
