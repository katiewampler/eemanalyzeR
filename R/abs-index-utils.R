#interpolate absorbance data to 1 nm intervals
abs_interp <- lapply(abslist, function(x){
  abs <- data.frame(x$data)
  abs_filled <- merge(abs, data.frame(X1=min(abs$X1):max(abs$X1)), all=T)
  abs_filled <- zoo::na.approx(abs_filled)
  x$data <- as.matrix(abs_filled)
  x$n <- nrow(abs_filled)
  return(x)
})
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
