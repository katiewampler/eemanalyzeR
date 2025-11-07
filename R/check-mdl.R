#' Check if a EEM sample is above the MDL
#'
#' Given a set of excitation and emission wavelengths, will check if the values in the
#' `eem` or `eemlist` are above the method detection limit (MDL)
#' for all excitation-emission pairs.
#'
#' @param eem an object of class \code{eem} or \code{eemlist}
#' @param abs an object of class \code{abs} or \code{abslist}
#' @param mdl a `eem` object containing MDL data, if no MDL provided will return NA
#' @param ex a vector of excitation wavelengths
#' @param em a vector of emission wavelengths
#' @param wl a vector of absorbance wavelengths
#' @param vals logical, if FALSE will return a TRUE or FALSE, if TRUE will
#' return a table with the eem and mdl values for the ex/em pair
#' @md
#' @rdname check-mdl
#' @returns
#' If `vals` is FALSE:
#'  - MDL01 if all values are below the MDL
#'  - MDL02 if some of the values are below the MDL
#'  - NA if all are above the MDL
#'
#' If `vals` is TRUE:
#'  - a `data.frame` with four columns:
#'    - ex: the excitation wavelengths
#'    - em: the emission wavelengths
#'    - eem: the EEM fluorescence value for the ex/em pair
#'    - mdl: the MDL for the ex/em pair
#' @export
#'
#' @examples
#' #get MDL data
#' eem_mdl <- readRDS(file.path(system.file("extdata", package = "eemanalyzeR"),
#' "eem-mdl.rds"))
#' abs_mdl <- readRDS(file.path(system.file("extdata", package = "eemanalyzeR"),
#' "abs-mdl.rds"))
#'
#' #works with a single sample
#' check_eem_mdl(example_processed_eems[[1]], eem_mdl, ex = 270:280, em=300:320)
#'
#' #or an eemlist
#' check_eem_mdl(example_processed_eems, eem_mdl, ex = 270:280, em=300:320)
#'
#' check_abs_mdl(example_processed_abs[[2]], abs_mdl, wl=254)
#' check_abs_mdl(example_processed_abs, abs_mdl, wl=254)

check_eem_mdl <- function(eem, mdl=NULL, ex, em,  vals = FALSE){
  #if no mdl, return NA so we can run the function without checking if we have mdl or not

  #has eem been blank subtracted and raman normalized?
    stopifnot(all(is.numeric(ex)), all(is.numeric(em)),
              .is_eem(eem) | .is_eemlist(eem))

  if(.is_eemlist(eem)){
    stopifnot(all(sapply(eem, attr, which= "is_raman_normalized")),
              all(sapply(eem, attr, which= "is_blank_corrected")))

    if(vals){
      mdl_table <- lapply(eem, function(x){
        table <- check_eem_mdl(x, mdl, ex, em, vals)
        table$sample <- x$meta_name
        return(table)
        })  %>% dplyr::bind_rows()
      return(mdl_table)
      }

    above_mdl <- sapply(eem,  check_eem_mdl, mdl=mdl, vals=vals, ex=ex, em=em)
    return(above_mdl)
  }

  #return NA if no mdl data is provided
  if(is.null(mdl)){return(NA)}

  #ensure processing has been done so the mdl is the same units
  stopifnot(attr(eem, "is_raman_normalized"),
            attr(eem, "is_blank_corrected"))

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

#' @export
#' @rdname check-mdl
check_abs_mdl <- function(abs, mdl=NULL, wl, vals = FALSE){
  stopifnot(all(is.numeric(wl)),
            .is_abs(abs) | .is_abslist(abs))

  if(.is_abslist(abs)){
    if(vals){
      mdl_table <- lapply(abs, function(x){
        table <- check_abs_mdl(x, mdl, wl, vals)
        table$sample <- x$meta_name
        return(table)
      })  %>% dplyr::bind_rows()
      return(mdl_table)
    }

    above_mdl <- sapply(abs,check_abs_mdl, mdl=mdl, vals=vals, wl=wl)
    return(above_mdl)
  }

  #return NA if no mdl data is provided
  if(is.null(mdl)){return(NA)}

  #interpolate mdl to make sure it matches the sample and index requested
    mdl_val <- abs_interp(mdl) %>% get_sample_info("data") %>% as.data.frame() %>%
      dplyr::rename("wl" = "X1", "mdl" = "X2") %>%
      dplyr::filter(.data$wl %in% !!wl)

  #get same values in eem, interpolate if needed
    abs_val <- abs_interp(abs) %>% get_sample_info("data") %>% as.data.frame() %>%
      dplyr::rename("wl" = "X1", "abs" = "X2") %>%
      dplyr::filter(.data$wl %in% !!wl)

  #make into a table
  mdl_table <- merge(abs_val, mdl_val, by=c("wl"))
  if(vals){return(mdl_table)} #return if this is requested

  #otherwise return NA or MDL01 flag
  if(all(na.omit(mdl_table$abs > mdl_table$mdl))){return(NA)} #if all non NA above MDL -> no flag needed
  if(all(na.omit(mdl_table$abs < mdl_table$mdl))){return("MDL01")} #if all values below MDL -> flag with MDL01
  if(any(na.omit(mdl_table$abs < mdl_table$mdl))){return("MDL02")} #if any values are below MDL -> flag with MDL02
}
