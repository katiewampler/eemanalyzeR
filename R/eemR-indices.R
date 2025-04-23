#' eemR and staRdom method for fluorescence and absorbance indices
#'
#' Calculates commonly used absorbance and fluorescence optical indices from eemlist and abslist using
#' \href{https://CRAN.R-project.org/package=eemR}{eemR} and \href{https://CRAN.R-project.org/package=staRdom}{staRdom} functions.
#'
#' @importFrom zoo na.approx
#' @importFrom tidyr pivot_wider
#'
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
  peaks <- list(pB = list(ex=270:280, em=300:320, all=FALSE),
                pT = list(ex=270:280, em=320:350, all=FALSE),
                pA = list(ex=250:260, em=380:480, all=FALSE),
                pM = list(ex=310:320, em=380:420, all=FALSE),
                pC = list(ex=330:350, em=420:480, all=FALSE),
                pD = list(ex=390, em=509, all=TRUE),
                pE = list(ex=455, em=521, all=TRUE),
                pN = list(ex=280, em=370, all=TRUE),
                rAT = list(ex=c(250:260,270:280), em=c(380:480,320:350), all=TRUE),
                rCA = list(ex=c(250:260,330:350), em=c(380:480,420:480), all=TRUE),
                rCM = list(ex=c(330:350,310:320), em=c(420:480,380:420), all=TRUE),
                rCT = list(ex=c(330:350,270:280), em=c(420:480,320:350), all=TRUE),
                pB_DOCnorm = list(ex=270:280, em=300:320, all=FALSE),
                pT_DOCnorm = list(ex=270:280, em=320:350, all=FALSE),
                pA_DOCnorm = list(ex=250:260, em=380:480, all=FALSE),
                pM_DOCnorm = list(ex=310:320, em=380:420, all=FALSE),
                pC_DOCnorm = list(ex=330:350, em=420:480, all=FALSE),
                pD_DOCnorm = list(ex=390, em=509, all=TRUE),
                pE_DOCnorm = list(ex=455, em=521, all=TRUE),
                pN_DOCnorm = list(ex=280, em=370, all=TRUE),
                FI = list(ex=370, em=c(470, 520), all=TRUE),
                HIX = list(ex=254, em=c(300:345,435:480), all=TRUE),
                HIX_ohno = list(ex=254, em=c(300:345,435:480), all=TRUE),
                fresh = list(ex=310, em=c(380, 420:435), all=TRUE),
                BIX = list(ex=310, em=c(380, 430), all=TRUE))

  #helper functions to get fluorescence index functions
  #only coble peaks are different if DOC normalized or not
  #interpolates to 1 nm and gives max value in that range

  #DATA_03: Unable to calculate ratio because denominator was zero
  peak_max <- function(eem, ex, em){
    ex_p <- rep(ex, length(em)) #gives values to interpolate between
    em_p <- rep(em, length(ex)) #gives values to interpolate between
    int_res <- pracma::interp2(eem$ex, eem$em, eem$x, ex_p, em_p)
    if(all(is.na(int_res))){
      max_res <- NA
    }else{
      max_res <- max(int_res, na.rm=T)
    }
    return(max_res)
  }
  get_ratios <- function(p1, p2){
    if(is.na(p1) | is.na(p2)){
      return("DATA_01")
    }else if(p2 == 0){
      return("DATA_03")
    }else{
      return(unname(p1 / p2))
    }
  }
  calc_peaks <- function(eem, doc=FALSE){

    #return DOC code if need DOC to normalize and not added
    if(doc & !.meta_added(eem)){pvals <- rep("DOC_01", 8)}

    #otherwise transform pvals as needed
    if(!doc){
      pvals <- sapply(peaks[1:8], function(p) peak_max(eem, p$ex, p$em))
      pvals <- c(pvals,
                 rAT = get_ratios(pvals["pA"], pvals["pT"]),
                 rCA = get_ratios(pvals["pC"], pvals["pA"]),
                 rCM = get_ratios(pvals["pC"], pvals["pM"]),
                 rCT = get_ratios(pvals["pC"], pvals["pT"]))
    }else{
      #get peaks
      pvals <- sapply(peaks[13:20], function(p) peak_max(eem, p$ex, p$em))
      pvals <- pvals / eem$doc_mgL
    }

    return(pvals)
  }


  calc_FI <- function(eem){
    f_470 <- pracma::interp2(eem$ex, eem$em, eem$x, 370, 470)
    f_520 <- pracma::interp2(eem$ex, eem$em, eem$x, 370, 520)

    FI <- get_ratios(f_470, f_520)

    return(FI)
  }
  calc_HIX <- function(eem, type="zsolnay"){
    em_high <- 435:480
    em_low <- 300:345
    ex_254 <- rep(254, length(em_low))
    sum_high <- sum(pracma::interp2(eem$ex, eem$em, eem$x,
                                    ex_254, em_high))
    sum_low <- sum(pracma::interp2(eem$ex, eem$em, eem$x,
                                   ex_254, em_low))

    if(type == "ohno"){
      HIX <- get_ratios(sum_high,(sum_low + sum_high))
    }else{HIX <- get_ratios(sum_high,sum_low)}
    return(HIX)
  }
  calc_fresh <- function(eem){
    f_380 <- pracma::interp2(eem$ex, eem$em, eem$x, 310, 380)
    f_420_435 <- peak_max(eem, 310, 420:435)

    fresh <- get_ratios(f_380, f_420_435)

    return(fresh)
  }
  calc_BIX <- function(eem){
    f_380 <- pracma::interp2(eem$ex, eem$em, eem$x, 310, 380)
    f_430 <- pracma::interp2(eem$ex, eem$em, eem$x, 310, 430)

    BIX <- get_ratios(f_380, f_430)
    return(BIX)
  }

  #get fluorescence indices
  eem_index <- do.call("rbind", lapply(eemlist, function(eem){
    vals <- c(calc_peaks(eem),
              calc_peaks(eem, doc=TRUE),
              FI = calc_FI(eem),
              HIX = calc_HIX(eem),
              HIX_ohno = calc_HIX(eem, "ohno"),
              fresh = calc_fresh(eem),
              BIX = calc_BIX(eem))
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
  abs_wl <- list(SUVA254 = 254, SUVA280=280, SVA412 = 412,
                 S_275_295 = 275:295, S_350_400=350:400,
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
    if(!is.numeric(S275_295)){S275_295 <- "DATA_04"}
    if(!is.numeric(S350_400)){S350_400 <- "DATA_04"}

    if(S275_295 == "DATA_04" |S350_400 == "DATA_04" ){
      SR <- "DATA_04"}else{
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
