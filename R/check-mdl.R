#' Check if a EEM sample is above the MDL
#'
#' Given a set of excitation and emission wavelengths, will check if the values in the
#' `eem` or `eemlist` are above the method detection limit (MDL)
#' for all excitation-emission pairs.
#'
#' @param eem an \code{eem} or \code{eemlist} object containing EEM's data
#' @param mdl a `eem` object containing MDL data
#' @param ex a vector of excitation wavelengths
#' @param em a vector of emission wavelengths
#' @param vals logical, if FALSE will return a TRUE or FALSE, if TRUE will
#' return a table with the eem and mdl values for the ex/em pair
#' @md
#' @returns
#' If `vals` is FALSE:
#'  - TRUE if all values are above the MDL
#'  - FALSE if one or more values within the range are below the MDL.
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
#' abslist <- add_metadata(metadata, example_absorbance)
#' eemlist <- add_metadata(metadata, example_eems)
#' eemlist <- add_blanks(eemlist, validate=FALSE)
#' eemlist <- process_eem(eemlist, abslist)
#' mdl <- readRDS(file.path(system.file("extdata", package = "eemanalyzeR"),
#' "eem-mdl.rds"))
#'
#' #works with a single sample
#' check_eem_mdl(eemlist[[1]], mdl, ex = 270:280, em=300:320)
#'
#' #or an eemlist
#' check_eem_mdl(eemlist, mdl, ex = 270:280, em=300:320)

check_eem_mdl <- function(eem, mdl, ex, em, vals = FALSE){
  #has eem been blank subtracted and raman normalized?
    stopifnot(all(is.numeric(ex)), all(is.numeric(em)), .is_eem(mdl),
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

  #ensure processing has been done so the mdl is the same units
  stopifnot(attr(eem, "is_raman_normalized"),
            attr(eem, "is_blank_corrected"))

  #get portions of mdl to check (based on wavelengths in eem)
    ex <- eem$ex[eem$ex >= min(ex, na.rm = TRUE) & eem$ex <= max(ex, na.rm = TRUE)]
    em <- eem$em[eem$em >= min(em, na.rm = TRUE) & eem$em <= max(em, na.rm = TRUE)]

  #interpolate mdl to make sure it matches the sample
    if(all(eem$ex %in% mdl$ex) & all(eem$em %in% mdl$em)){
      mdl_val <- eem_flatten(mdl) %>% dplyr::rename("mdl" = "fluor") %>%
        dplyr::filter(.data$ex %in% !!ex & .data$em %in% !!em)
    }else{
      ex_p <- rep(ex, length(em)) #gives values to interpolate between
      em_p <- rep(em, length(ex)) #gives values to interpolate between
      mdl_val <- data.frame(ex = ex_p, em = em_p,
                            mdl = pracma::interp2(mdl$ex, mdl$em, mdl$x, ex_p, em_p))
    }

  #get same values in eem and see if they're all larger
    eem_val <- eem_flatten(eem) %>% dplyr::filter(.data$ex %in% !!ex & .data$em %in% !!em)

  #make into a table
    mdl_table <- merge(eem_val, mdl_val, by=c("ex", "em"))
    if(vals){return(mdl_table)} #return if this is requested

  #otherwise return T/F
    above_mdl <- all(na.omit(mdl_table$fluor > mdl_table$mdl))

    return(above_mdl)
}
