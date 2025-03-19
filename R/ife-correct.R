#' Perform Inner-Filter Corrections
#'
#' A wrapper for eemR function \link[eemR]{eem_inner_filter_effect},
#' modified to work with the augmented eemlist structure and automatically
#' cut EEM's wavelengths down to match absorbance data instead of returning an error.
#'
#' @details
#' The function links the absorbance and EEM's data by the metadata
#' name which should be the same between the two datasets, because of this,
#' samples must have metadata already added to the samples using \link[eemanalyzeR]{add_metadata}.
#'
#' @param eemlist an \code{eemlist} object containing EEM's data. See details for more info.
#' @param abslist an \code{abslist} object containing absorbance data.
#' @param pathlength a numeric value indicating the pathlength (in cm) of the
#'   cuvette used for absorbance measurement. Default is 1 (1 cm).
#'
#' @returns an object of class \code{eemlist}
#' @export
#'
#' @importFrom eemR eem_inner_filter_effect eem_cut
#'
#' @source
#' Massicotte P (2019). eemR: Tools for Pre-Processing
#' Emission-Excitation-Matrix (EEM) Fluorescence Data. R package
#' version 1.0.1, \href{https://CRAN.R-project.org/package=eemR}{https://CRAN.R-project.org/package=eemR}
#'
#' @examples
#' eemlist <- add_metadata(metadata,example_eems)
#' abslist <- add_metadata(metadata, example_absorbance)
#' correct_eem <- ife_correct(eemlist, abslist)

ife_correct <- function(eemlist, abslist, pathlength=1){
  if(!any(.meta_added(eemlist), .meta_added(abslist))){
    stop("metadata must be added to eemlist and abslist to link samples. \nPlease add metadata using 'add_metadata' function")
  }
  #internal function from eemR, added here to maintain stability
  is_between <- function (x, a, b) {x >= a & x <= b }

  abs_table <- get_sample_info(abslist,"data")
  colnames(abs_table)[-1] <- get_sample_info(abslist, "meta_name")

  #clip eems if necessary
    ex <- unique(as.vector(get_sample_info(eemlist, "ex")))
    em <- unique(as.vector(get_sample_info(eemlist, "em")))
    abs <- abs_table$wavelength
    ex_rm <- ex[!(ex  >= min(abs) & ex <= max(abs))]
    em_rm <- em[!(em >= min(abs) & em <= max(abs))]

    if(length(ex_rm) >0|length(em_rm) >0){
      eemlist <- eemR::eem_cut(eemlist, ex_rm, em_rm, exact=T)
      warning("trimmed EEM's to match absorbance data wavelengths, see readme.txt for more info")
      trim <- TRUE}else{trim <- FALSE}


    #not an ideal solution, but for now in function only,
    #replace sample name with meta_name for matching and remove extra items
    res <- lapply(eemlist, .make_base_eem)
    class(res) <- "eemlist"

    res <- eemR::eem_inner_filter_effect(res, abs_table, pathlength = pathlength)

    #put inner filter effect corrected eems back into augmented eemlist
    res <- lapply(1:length(eemlist), function(i){
      eemlist[[i]]$x <- res[[i]]$x
      attr(eemlist[[i]], "is_ife_corrected") <- TRUE
      return(eemlist[[i]])
    })

    #write processing to readme
    .write_readme_line("data was corrected for inner filter effects via 'ife_correct' function")
    if(trim){
      ex_range <- ifelse(length(ex_rm)>0, paste0(range(ex_rm), collapse=" - "),"")
      em_range <- ifelse(length(em_rm)>0, paste0(range(em_rm), collapse=" - "),"")
      assign("readme", c(readme,
                         paste0("   warning: trimmed EEM's to match absorbance data wavelengths\n\texcitation: ",
                         ex_range, "\n\temission: ", em_range, "\n")), envir = .GlobalEnv)}
    class(res) <- "eemlist"

    return(res)
}
