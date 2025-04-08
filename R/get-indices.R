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
eemanalyzeR_indices <- function(eemlist){

    #coble peaks
    peaks <- list(pB = list(ex=270:280, em=300:320),
                  pT = list(ex=270:280, em=320:350),
                  pA = list(ex=250:260, em=380:480),
                  pM = list(ex=310:320, em=380:420),
                  pC = list(ex=330:350, em=420:480),
                  pD = list(ex=390, em=509),
                  pE = list(ex=455, em=521),
                  pN = list(ex=280, em=370))

    #get peaks
    index <- sapply(eemlist, function(eem){
      coble <- sapply(peaks, function(x){
        peak_val <- peak_max(eem, x$ex, x$em)
        return(peak_val)
      })
      return(coble)
  }, simplify = F)

    #get ratios
    index <- sapply(index, function(x){
      ratio <- c(x, rAT = unname(x["pA"] / x["pT"]),
                  rCA = unname(x["pC"] / x["pA"]),
                  rCM = unname(x["pC"] / x["pM"]),
                  rCT = unname(x["pC"] / x["pT"]))
      return(ratio)
    })

    #other fluorescence peaks
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

    index <- rbind(index, FI = fi, HIX=hix, HIX_ohno=hix_o,
                   fresh=fresh, BIX=bix)

    #absorbance peaks






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
