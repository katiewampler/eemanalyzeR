

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
calc_peaks <- function(eem){
  pvals <- sapply(peaks[1:8], function(p) pracma::interp2(eem$ex, eem$em, eem$x, p$ex, p$em))}
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
