#' USGS methods for fluorescence and absorbance indices
#'
#' Calculates commonly used absorbance and fluorescence optical indices from eemlist and abslist.
#' Reports indices commonly used by the U.S. Geological Survey.
#' For detailed descriptions and references for indices, Hansen et. al. 2018 (Table 1 and 8).
#'
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
#'  \item index: the name of the index being reported, the index name is a human readable index name followed by the USGS parameter code.
#'  \item value: the value of the index
#' }
#'
#' @export
#'
#' @source
#' Hansen, A. M., Fleck, J., Kraus, T. E. C., Downing, B. D., von Dessonneck, T., & Bergamaschi, B. (2018).
#' Procedures for using the Horiba Scientific Aqualog® fluorometer to measure absorbance and fluorescence from dissolved organic matter
#' (USGS Numbered Series No. 2018–1096). Procedures for using the Horiba Scientific Aqualog® fluorometer to measure absorbance and fluorescence
#' from dissolved organic matter (Vol. 2018–1096). Reston, VA: U.S. Geological Survey. \url{https://doi.org/10.3133/ofr20181096}
#'
#' @examples
#' abslist <- add_metadata(metadata, example_absorbance)
#' eemlist <- add_metadata(metadata, example_eems)
#' indices <- usgs_indices(eemlist, abslist)
usgs_indices <- function(eemlist, abslist, cuvle=1){
  stopifnot(.is_eemlist(eemlist), .is_abslist(abslist), is.numeric(cuvle), all(sapply(eemlist, attr, "is_doc_normalized"))==FALSE)

  #get fluoresence peaks
    #define wavelengths for peaks and metrics to check if there are missing wavelengths
    #format: index = list(excitation wavelengths, emission wavelengths, do all wavelengths need to exist to return value?)
    peaks <- list(pA_32304 = list(ex=260,em=450, all=TRUE),
                  pB_32305 = list(ex=275,em=304, all=TRUE),
                  pC_52901 = list(ex=340,em=440, all=TRUE),
                  pD_32307 = list(ex=390,em=510, all=TRUE),
                  FDOM_52902 = list(ex=370,em=460, all=TRUE),
                  pM_32309 = list(ex=300,em=390, all=TRUE),
                  pN_32310 = list(ex=280,em=370, all=TRUE),
                  pT_32311 = list(ex=275,em=340, all=TRUE),
                  FI_32312 = list(ex=370, em=c(470,520), all=TRUE),
                  HIX_32313 = list(ex=254, em=c(300:345, 435:480), all=TRUE))

    #helper functions to get fluorescence index functions
      #only coble peaks are different if DOC normalized or not
      #interpolates to 1 nm and gives max value in that range
      #DATA_03: Unable to calculate ratio because denominator was zero
      get_ratios <- function(p1, p2){
        if(is.na(p1) | is.na(p2)){
          return("DATA_01")
        }else if(p2 == 0){
          return("DATA_03")
        }else{
          return(unname(p1 / p2))
        }
      }
      calc_peaks <- function(eem){
        pvals <- sapply(peaks[1:8], function(p) pracma::interp2(eem$ex, eem$em, eem$x, p$ex, p$em))}
      calc_FI <- function(eem){
        f_470 <- pracma::interp2(eem$ex, eem$em, eem$x, 370, 470)
        f_520 <- pracma::interp2(eem$ex, eem$em, eem$x, 370, 520)

        FI <- get_ratios(f_470, f_520)

        return(FI)
      }
      calc_HIX <- function(eem){
        em_high <- 435:480
        em_low <- 300:345
        ex_254 <- rep(254, length(em_low))
        sum_high <- sum(pracma::interp2(eem$ex, eem$em, eem$x,
                                        ex_254, em_high))
        sum_low <- sum(pracma::interp2(eem$ex, eem$em, eem$x,
                                       ex_254, em_low))

        HIX <- get_ratios(sum_high,sum_low)
        return(HIX)
      }

    #get fluorescence indices
      eem_index <- do.call("rbind", lapply(eemlist, function(eem){
        vals <- unname(c(calc_peaks(eem),
                  FI= calc_FI(eem),
                  HIX = calc_HIX(eem)))
        names(vals) <- names(peaks)
        name_type <- ifelse(.meta_added(eem), "meta_name", "sample")
        index <- data.frame(sample_name= get_sample_info(eem, "sample"),
                            meta_name=get_sample_info(eem, name_type),
                            index = names(vals),
                            value = unname(vals))}))

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
    abs_wl <- list(A254_50624 = 254,
                   SUVA254_63162 = 254,
                   A280_32296 =280,
                   A370_32297 = 370,
                   A412_32298 = 412,
                   A440_32299 = 440,
                   S275_295_32300 = 275:295,
                   S290_350_32301 = 290:350,
                   S350_400_32302 =350:400,
                   S412_600_32331 = 412:600)

  #helper functions to get absorbance indices
    calc_abs <- function(abs, wl, doc=FALSE){
        abs_val <- unname(abs$data[abs$data[,1] == wl,2]) *  (1/cuvle)
        if(doc){
          if(.meta_added(abs)){
            #absorbance/ DOC * 100 (to correct for cuvette length and get in m)
            abs_val <- abs_val /abs$doc_mgL * 100
          }else{
            abs_val <- NA
          }}
        return(abs_val)}
    #DATA_04: Spectral slope was unable to be calculated
    calc_ratios <- function(abs){
      #get absorption in m^-1 (convert from absorbance to absorption based on Beer's Law)
      absorption <- abs$data[,2] * log(10) / (cuvle/100) #convert cm cuvette to m

      S275_295 <- staRdom::abs_fit_slope(abs$data[,1], absorption,
                                         lim=c(275,295), l_ref=275)$coefficient
      S290_350 <- staRdom::abs_fit_slope(abs$data[,1], absorption,
                                         lim=c(290,350), l_ref=412)$coefficient
      S350_400 <- staRdom::abs_fit_slope(abs$data[,1], absorption,
                                         lim=c(350,400), l_ref=350)$coefficient
      S412_600 <- staRdom::abs_fit_slope(abs$data[,1], absorption,
                                         lim=c(412,600), l_ref=412)$coefficient

      #prevent non numeric values
      if(!is.numeric(S275_295)){S275_295 <- "DATA_04"}
      if(!is.numeric(S275_295)){S275_295 <- "DATA_04"}
      if(!is.numeric(S350_400)){S350_400 <- "DATA_04"}
      if(!is.numeric(S412_600)){S350_400 <- "DATA_04"}


      vals <- unname(c(S275_295,S290_350, S350_400, S412_600)) #remove previous names
      names(vals) <- c("S275_295", "S290_350", "S350_400", "S412_600")

      return(vals)
    }

  #get all indices
    abs_index <- do.call("rbind", lapply(abs_interp, function(abs){
      vals <- unname(c(calc_abs(abs,abs_wl[1]),
                calc_abs(abs,abs_wl[2], doc=TRUE),
                calc_abs(abs,abs_wl[3]),
                calc_abs(abs,abs_wl[4]),
                calc_abs(abs,abs_wl[5]),
                calc_abs(abs,abs_wl[6]),
                calc_ratios(abs)))
      names(vals) <- names(abs_wl)
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
