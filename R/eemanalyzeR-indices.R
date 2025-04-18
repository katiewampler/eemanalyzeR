# TODO: Need a LOT more documentation of these different indices, write to readme
#' Default package methods for fluorescence and absorbance indices
#'
#'
#'
#' @importFrom zoo na.spline
#' @importFrom tidyr pivot_wider
#' @param eemlist an \code{eemlist} object containing EEM's data. See details for more info.
#' @param abslist an \code{abslist} object containing absorbance data.
#' @param return either "long" or "wide" to specify the format of the indices data.frames
#' @param cuvle cuvette (path) length in cm
#' @note If absorbance is not at a 1 nanometer interval, absorbance will be interpolated using \link[zoo]{na.spline} which fills in missing values
#' using spline interpolation.
#'
#' @return a list with two objects:
#' \itemize{
#'   \item eem_index: a data.frame of all the fluorescence indices
#'   \item abs_index: a data.frame of all the absorbance indices
#' }
#' If \code{return} is "long" each data.frame will have four columns:
#' \itemize{
#'  \item sample_name: the name of the sample
#'  \item meta_name: the name of the sample in the metadata if metadata has been added, otherwise the sample name again
#'  \item metric: the name of the index being reported, see details for more information.
#'  \item value: the value of the index
#'  \item QAQC_flag: any flags associated with the data. See details for more information.
#' }
#'
#' @export
#'
#' @examples
#' abslist <- add_metadata(metadata, example_absorbance)
#' eemlist <- add_metadata(metadata, example_eems)
#' indices <- eemanalyzeR_indices(eemlist, abslist)
eemanalyzeR_indices <- function(eemlist, abslist, cuvle=1, return="long"){
  #get fluoresence peaks
  #define wavelengths for coble peaks
  peaks <- list(pB = list(ex=270:280, em=300:320),
                pT = list(ex=270:280, em=320:350),
                pA = list(ex=250:260, em=380:480),
                pM = list(ex=310:320, em=380:420),
                pC = list(ex=330:350, em=420:480),
                pD = list(ex=390, em=509),
                pE = list(ex=455, em=521),
                pN = list(ex=280, em=370))

  #helper functions to flag data [maybe move to broader function to flag no matter what function used]
  missing_doc_flag <- function(index){
    doc_flag <- grepl("SUVA|SVA|DOC", index$index) & is.na(index$value)
    index$QAQC_flag[doc_flag] <- "DOC_01"
    return(index)
  }

  #helper functions to get fluorescence index functions
  #only coble peaks are different if DOC normalized or not
  #interpolates to 1 nm and gives max value in that range
  peak_max <- function(eem, ex, em){
    ex_p <- rep(ex, length(em)) #gives values to interpolate between
    em_p <- rep(em, length(ex)) #gives values to interpolate between
    int_res <- pracma::interp2(eem$ex, eem$em, eem$x, ex_p, em_p)
    max_res <- max(int_res, na.rm=T)
    return(max_res)
  }

  calc_peaks <- function(eem, doc=FALSE){
    if(doc){
      pvals <- sapply(peaks, function(p) peak_max(eem, p$ex, p$em))
      if(.meta_added(eem)){pvals_norm <- pvals / eem$doc_mgL}else{pvals_norm <- rep(NA, length(pvals))}
      names(pvals_norm) <- paste0(names(pvals_norm), "_DOCnorm")
      pvals <- pvals_norm
    }else{
      pvals <- sapply(peaks, function(p) peak_max(eem, p$ex, p$em))
      pvals <- c(pvals,
                 rAT = unname(pvals["pA"] / pvals["pT"]),
                 rCA = unname(pvals["pC"] / pvals["pA"]),
                 rCM = unname(pvals["pC"] / pvals["pM"]),
                 rCT = unname(pvals["pC"] / pvals["pT"]))
    }

    return(pvals)
  }

  calc_FI <- function(eem){
    f_470 <- pracma::interp2(eem$ex, eem$em, eem$x, 370, 470)
    f_520 <- pracma::interp2(eem$ex, eem$em, eem$x, 370, 520)
    FI <- f_470/f_520
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

    if(type == "ohno"){HIX <- sum_high/(sum_low + sum_high)}else{HIX <- sum_high/(sum_low)}
    return(HIX)
  }

  calc_fresh <- function(eem){
    f_380 <- pracma::interp2(eem$ex, eem$em, eem$x, 310, 380)
    f_420_435 <- peak_max(eem, 310, 420:435)
    fresh <- f_380/f_420_435
    return(fresh)
  }

  calc_BIX <- function(eem){
    f_380 <- pracma::interp2(eem$ex, eem$em, eem$x, 310, 380)
    f_430 <- pracma::interp2(eem$ex, eem$em, eem$x, 310, 430)
    BIX <- f_380/f_430
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

  #add flags and change NA to -9999
  #TODO: flag potential high or low values, values where reporting just noise
  eem_index <- missing_doc_flag(eem_index)
  eem_index[is.na(eem_index)] <- -9999

  #absorbance peaks
  #interpolate absorbance data to 1 nm intervals
  abs_interp <- lapply(abslist, function(x){
    abs <- data.frame(x$data)
    abs_filled <- merge(abs, data.frame(X1=min(abs$X1):max(abs$X1)), all=T)
    abs_filled <- zoo::na.spline(abs_filled)
    x$data <- as.matrix(abs_filled)
    x$n <- nrow(abs_filled)
    return(x)
  })

  #specify SUVA peaks
  suva_wl <- list(SUVA254 = 254, SUVA280=280, SUVA350=350,
                  SUVA370 = 370, SVA412 = 412, SVA440 = 440,
                  SVA510 = 510, SVA532=532, SVA555=555)

  #get SVA and SUVA values, if no DOC, return NA
  calc_suva <- function(abs){
    suva_metrics <- sapply(suva_wl, function(wl){
      if(.meta_added(abs)){
        abs_val <- unname(abs$data[abs$data[,1] == wl,2]) /abs$doc_mgL
      }else{
        abs_val <- NA
      }})
    return(suva_metrics)}
  calc_ratios <- function(abs){
    #get absorption in m^-1 (convert from absorbance to absorption based on Beer's Law)
    absorption <- abs$data[,2] * log(10) / (cuvle/100) #convert cm cuvette to m

    S275_295 <- staRdom::abs_fit_slope(abs$data[,1], absorption,
                                   lim=c(275,295), l_ref=275)$coefficient
    S350_400 <- staRdom::abs_fit_slope(abs$data[,1], absorption,
                                   lim=c(350,400), l_ref=350)$coefficient

    SR <- S275_295 / S350_400

    E2_E3 <- abs$data[abs$data[,1]==250,2] / abs$data[abs$data[,1]==365,2]

    E4_E6 <- abs$data[abs$data[,1]==465,2] / abs$data[abs$data[,1]==665,2]

    vals <- unname(c(S275_295, S350_400, SR, E2_E3, E4_E6)) #remove previous names
    names(vals) <- c("S275_295", "S350_400", "SR", "E2_E3", "E4_E6")

    return(vals)
  }

  abs_index <- do.call("rbind", lapply(abs_interp, function(abs){
    vals <- c(calc_suva(abs), calc_ratios(abs))
    name_type <- ifelse(.meta_added(abs), "meta_name", "sample")
    index <- data.frame(sample_name= get_sample_info(abs, "sample"),
                        meta_name=get_sample_info(abs, name_type),
                        index = names(vals),
                        value = unname(vals))}))

  abs_index <- missing_doc_flag(abs_index)


  abs_index$value[is.na(abs_index$value)] <- -9999

  #flag as needed for ratios with low noise,negative numbers
  #TODO: add flags, negative numbers


  #return list of index values
  if(return == "wide"){
    #TODO: will need to figure out combining strings later
    abs_index <- tidyr::pivot_wider(abs_index, names_from="metric", values_from="value")
    eem_index <- tidyr::pivot_wider(abs_index, names_from="metric", values_from="value")

  }

  index <- list(eem_index = eem_index, abs_index=abs_index)


}
