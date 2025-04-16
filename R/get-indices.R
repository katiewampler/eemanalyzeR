# TODO add description of what this does, add details linking to the sources of the different
# index functions

#' Get preset or use custom functions to generate fluorescence and absorbance indices
#'
#' @param index_method currently supports "eemanalyzeR", "eemR", and "usgs". See details for more information.
#'
#' @returns a function used to generate indices
#' @export
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

#interpolates to 1 nm and gives max value in that range
peak_max <- function(eem, ex, em){
  ex_p <- rep(ex, length(em)) #gives values to interpolate between
  em_p <- rep(em, length(ex)) #gives values to interpolate between
  int_res <- pracma::interp2(eem$ex, eem$em, eem$x, ex_p, em_p)
  max_res <- max(int_res, na.rm=T)
  return(max_res)
}


#' Default package methods for indices
#'
#' @importFrom zoo na.spline
#' @param eemlist an \code{eemlist} object containing EEM's data. See details for more info.
#' @param abslist an \code{abslist} object containing absorbance data.
#' @note If absorbance is not at a 1 nanometer interval, absorbance will be interpolated using \link[zoo]{na.spline} which fills in missing values
#' using spline interpolation.
#'
#' @return
#' @export
#'
#' @examples
#' abslist <- add_metadata(metadata, abslist)
#' eemlist <- add_metadata(metadata, eemlist)
eemanalyzeR_indices <- function(eemlist, abslist){
  #TODO: warn if processing steps haven't been done

  #get fluoresence peaks
    #list out the wavelengths for coble peaks
    peaks <- list(pB = list(ex=270:280, em=300:320),
                  pT = list(ex=270:280, em=320:350),
                  pA = list(ex=250:260, em=380:480),
                  pM = list(ex=310:320, em=380:420),
                  pC = list(ex=330:350, em=420:480),
                  pD = list(ex=390, em=509),
                  pE = list(ex=455, em=521),
                  pN = list(ex=280, em=370))

    #loop across list of peaks and list of eem samples to get coble peak values
    #TODO: also return DOC normalized values here??
    eem_index <- sapply(eemlist, function(eem){
      coble <- sapply(peaks, function(x){
        peak_val <- peak_max(eem, x$ex, x$em)
        return(peak_val)
      })
      return(coble)
  }, simplify = F)

    #get ratios from the peak values
    eem_index <- sapply(eem_index, function(x){
      ratio <- c(x, rAT = unname(x["pA"] / x["pT"]),
                  rCA = unname(x["pC"] / x["pA"]),
                  rCM = unname(x["pC"] / x["pM"]),
                  rCT = unname(x["pC"] / x["pT"]))
      return(ratio)
    })

    #get other fluorescence peaks
    fi <- sapply(eemlist, function(eem){
      f_470 <- pracma::interp2(eem$ex, eem$em, eem$x, 370, 470)
      f_520 <- pracma::interp2(eem$ex, eem$em, eem$x, 370, 520)
      FI <- f_470/f_520
      return(FI)
    })
    hix <- sapply(eemlist, function(eem){
      em_435_480 <- seq(from = 435, to = 480, by = 1)
      em_300_345 <- seq(from = 300, to = 345, by = 1)
      ex_254 <- rep(254, length(em_300_345))
      sum_em_435_480 <- sum(pracma::interp2(eem$ex, eem$em, eem$x,
                                            ex_254, em_435_480))
      sum_em_300_345 <- sum(pracma::interp2(eem$ex, eem$em, eem$x,
                                            ex_254, em_300_345))
      HIX <- sum_em_435_480/(sum_em_300_345)

    })
    hix_o <- sapply(eemlist, function(eem){
      em_435_480 <- seq(from = 435, to = 480, by = 1)
      em_300_345 <- seq(from = 300, to = 345, by = 1)
      ex_254 <- rep(254, length(em_300_345))
      sum_em_435_480 <- sum(pracma::interp2(eem$ex, eem$em, eem$x,
                                            ex_254, em_435_480))
      sum_em_300_345 <- sum(pracma::interp2(eem$ex, eem$em, eem$x,
                                            ex_254, em_300_345))
      HIX_o <- sum_em_435_480/(sum_em_300_345 + sum_em_435_480)

    })
    fresh <- sapply(eemlist, function(eem){
      f_380 <- pracma::interp2(eem$ex, eem$em, eem$x, 310, 380)
      f_420_435 <- peak_max(eem, 310, 420:435)
        fresh <- f_380/f_420_435
    })
    bix <- sapply(eemlist, function(eem){
      f_380 <- pracma::interp2(eem$ex, eem$em, eem$x, 310, 380)
      f_430 <- pracma::interp2(eem$ex, eem$em, eem$x, 310, 430)
      BIX <- f_380/f_430
    })

    #combine all index values together
    eem_index <- rbind(eem_index, FI = fi, HIX=hix, HIX_ohno=hix_o,
                   fresh=fresh, BIX=bix)

    #give names to columns
    if(all(sapply(eemlist, .meta_added))){
      colnames(eem_index) <- get_sample_info(eemlist, "meta_name")
    }else{
      colnames(eem_index) <- get_sample_info(eemlist, "sample")
    }

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

    #get SVA and SUVA values if DOC is added
      if(any(sapply(abslist, .meta_added))){

        suva <- list(SUVA254 = 254, SUVA280=280, SUVA350=350,
                     SUVA370 = 370, SVA412 = 412, SVA440 = 440,
                     SVA510 = 510, SVA532=532, SVA555=555)

        abs_index <- sapply(abs_interp, function(abs, doc){
          suva_metrics <- sapply(suva, function(x){
            abs_val <- unname(abs$data[abs$data[,1] == x,2]) /abs$doc_mgL
              return(abs_val)
          })})

        #TODO: add flag for missing DOC data

      }

    #get spectral ratios
    #TODO modify this code still
    S275_295 <- sapply(lapply(data[,-ncol(data)]*log(10)*100/cuvle, staRdom:::abs_fit_slope,
                              wl=data$wavelength, lim=c(275, 295),
                              l_ref=275), function(res) res$coefficients)

    S350_400 <- sapply(lapply(data[,-ncol(data)]*log(10)*100/cuvle, staRdom:::abs_fit_slope,
                              wl=data$wavelength, lim=c(350, 400),
                              l_ref=350), function(res) res$coefficients)


    abs_out$S275_295 <- as.numeric(S275_295)
    abs_out$S350_400 <- as.numeric(S350_400)

    abs_out$SR <- abs_out$S275_295 / abs_out$S350_400

    #give names to columns
    if(all(sapply(abslist, .meta_added))){
      colnames(abs_index) <- get_sample_info(abslist, "meta_name")
    }else{
      colnames(abs_index) <- get_sample_info(abslist, "sample")
    }

    #flag as needed for ratios with low noise
    #TODO: add flags


    #return list of index values
    index <- list(eem_index = eem_index, abs_index=abs_index)

  }

eemR_indices <- function(){
  print("eemR indices")
}

usgs_indices <- function(){
  print("usgs indices")
}

#' Get fluoresence and absorbance indices
#'
#' @param eemlist an \code{eemlist} object containing EEM's data. See details for more info.
#' @param abslist an \code{abslist} object containing absorbance data.
#' @param index_method currently supports "eemanalyzeR", "eemR", and "usgs". See \link[eemanalyzeR]{get_indices_function} for more information.

#' @return a data.frame with index values for each sample with a row for each sample
#' @export
#'
get_indices <- function(eemlist, abslist, index_method){

  #get function to get indices
  index_function <- get_indices_function(index_method)

  #get indices

  #flag if needed, put flags in place of value, mark NA values with why, if want a value do val_flag
    #check for ratios below noise, missing values due to no DOC or outside measurement range

  #return


}
