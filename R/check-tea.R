#' Check if a tea standard is consistent with long-term standard
#'
#' Calculate the indices in a tea standard to ensure they are consistent
#' with the long-term standard.
#'
#' @param x an object of class `eem`, `eemlist`, `abs`, or `abslist`, needs to be fully processed
#' @param eem_std a `eem` object containing the long-term average tea standards for fluorescence
#' @param abs_std an `abs` object containing the long-term average tea standards for absorbance
#' @param tolerance what is the maximum percentage the tea standard can vary from the long-term values without being flagged?
#' @param index_method the index method used to calculate indices, see \link[eemanalyzeR]{get_indices} for more info.
#' @param vals logical, if FALSE will return a flag, if TRUE will
#' return a table with the observed and mdl values for the ex/em pair
#' @md
#' @returns
#' If `vals` is FALSE:
#'  - STD01 if all values outside of the tolerance
#'  - STD02 if some of the values are outside of the tolerance
#'  - NA if all are within the tolerance
#'
#' If `vals` is TRUE:
#'  - a `data.frame` with the index, the observed value, the long-term standard value, and the percent deviation

check_tea_std <- function(x, eem_std, abs_std, tolerance=0.2, index_method="eemanalyzeR", vals = FALSE){
  stopifnot(.is_eem(eem_std), .is_abs(abs_std), is.numeric(tolerance),
            .is_eem(x)|.is_eemlist(x)|.is_abs(x)|.is_abslist(x))

  #get x attributes to make sure they're processed the same as the standard
    x_att <- attributes(x)
    x_att$names <- NULL

    if(.is_eem(x)){std_att <- attributes(eem_std)}
    if(.is_abs(x)){std_att <- attributes(abs_std)}

    std_att$names <- NULL
    stopifnot(setequal(std_att, x_att))

  #calculate indices for standard
    #make std a length 1 eemlist/abslist
    eem_std <- list(eem_std)
    class(eem_std) <- "eemlist"

    abs_std <- list(abs_std)
    class(abs_std) <- "abslist"
    index_function <- get_indices_function(index_method)

    std_index <- index_function(eem_std, abs_std)

  #calculate indices for tea



  #interpolate mdl to make sure it matches the sample and index requested
  if(all(eem$ex %in% mdl$ex) & all(eem$em %in% mdl$em) &
     all(ex %in% eem$ex) & all(em %in% eem$em)){
    mdl_val <- eem_transform(mdl) %>% dplyr::rename("mdl" = "fluor") %>%
      dplyr::filter(.data$ex %in% !!ex & .data$em %in% !!em)
  }else{
    ex_p <- rep(ex, length(em)) #gives values to interpolate between
    em_p <- rep(em, length(ex)) #gives values to interpolate between
    mdl_val <- data.frame(ex = ex_p, em = em_p,
                          mdl = pracma::interp2(mdl$ex, mdl$em, mdl$x, ex_p, em_p))
  }

  #get same values in eem, interpolate if needed
  if(all(ex %in% eem$ex) & all(em %in% eem$em)){
    eem_val <- eem_transform(eem) %>% dplyr::filter(.data$ex %in% !!ex & .data$em %in% !!em)
  }else{
    ex_p <- rep(ex, length(em)) #gives values to interpolate between
    em_p <- rep(em, length(ex)) #gives values to interpolate between
    eem_val <- data.frame(ex = ex_p, em = em_p,
                          fluor = pracma::interp2(eem$ex, eem$em, eem$x, ex_p, em_p))
  }

  #make into a table
  mdl_table <- merge(eem_val, mdl_val, by=c("ex", "em"))
  if(vals){return(mdl_table)} #return if this is requested

  #otherwise return NA or MDL01 flag
  if(all(na.omit(mdl_table$fluor > mdl_table$mdl))){return(NA)} #if all non NA above MDL -> no flag needed
  if(all(na.omit(mdl_table$fluor < mdl_table$mdl))){return("MDL01")} #if all values below MDL -> flag with MDL01
  if(any(na.omit(mdl_table$fluor < mdl_table$mdl))){return("MDL02")} #if any values are below MDL -> flag with MDL02
}
