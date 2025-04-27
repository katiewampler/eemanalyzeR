missing_data_flag <- function(index_wl, x){
  stopifnot(.is_abs(x) | .is_abslist(x) | .is_eem(x) | .is_eemlist(x), is.list(x), unlist(unique(lapply(index_wl, names))) == c("ex", "em", "all"))

  if(.is_abs(x)){

  }

  if(.is_abslist(x)){

  }

  if(.is_eem(x)){

  }

  if(.is_eemlist(x)){

  }

  return(flags)
}

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
