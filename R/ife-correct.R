#' Perform inner-filter correction on EEM data
#'
#' Wrapper around [eemR::eem_inner_filter_effect()] that supports the
#' augmented `eemlist` structure used in this package.
#' Unlike the base function, this version automatically trims EEM
#' wavelengths to match the available absorbance data instead of returning
#' an error.
#'
#' @details
#' Inner-filter correction requires linking each EEM to its corresponding
#' absorbance spectrum. This function uses the metadata `name` field to pair
#' samples, so **metadata must be added** beforehand using
#' [add_metadata()].
#'
#' @param eemlist An `eemlist` object.
#' @param abslist An `abslist` object.
#' @param cuvle Cuvette (path) length in cm.
#' @param arg_names Optional list of arguments passed from
#'   higher-level functions for README generation.
#'
#' @return
#' An `eemlist` with inner-filter–corrected EEMs.
#'
#' @md
#' @export
#'
#' @source
#' Massicotte P. (2019). *eemR: Tools for Pre-Processing
#' Emission-Excitation-Matrix (EEM) Fluorescence Data.* R package version
#' 1.0.1. <https://CRAN.R-project.org/package=eemR>
#'
#' @examples
#' eemlist <- add_metadata(metadata, example_eems)
#' abslist <- add_metadata(metadata, example_abs)
#' correct_eem <- ife_correct(eemlist, abslist)

ife_correct <- function(eemlist, abslist, cuvle=1, arg_names=NULL){
  if(!any(.meta_added(eemlist), .meta_added(abslist))){
    stop("metadata must be added to eemlist and abslist to link samples. \nPlease add metadata using 'add_metadata' function")
  }

  #collect arguments for readme, and to put into the following functions
  if(is.null(arg_names)){
    args <- rlang::enquos(cuvle)
    names(args) <- c("cuvle")
  }else{args <- arg_names}

  #internal function from eemR, added here to maintain stability
  is_between <- function (x, a, b) {x >= a & x <= b }

  abs_table <- get_sample_info(abslist,"data")
  colnames(abs_table)[-1] <- get_sample_info(abslist, "meta_name")
  abs_table <- as.data.frame(abs_table)

  #clip eems if necessary
    ex <- unique(as.vector(get_sample_info(eemlist, "ex")))
    em <- unique(as.vector(get_sample_info(eemlist, "em")))
    abs <- abs_table[["wavelength"]]
    ex_rm <- ex[!(ex  >= min(abs) & ex <= max(abs))]
    em_rm <- em[!(em >= min(abs) & em <= max(abs))]

    if(length(ex_rm) >0|length(em_rm) >0){
      eemlist <- eemR::eem_cut(eemlist, ex_rm, em_rm, exact=T)
      warning("trimmed EEM's to match absorbance data wavelengths, see readme.txt for more info")
      trim <- TRUE}else{trim <- FALSE}


    #modify eemR ife function to return warning and not print otherwise, also works now with modified eem format
    ife_eemR <- function(eem, absorbance, cuvle = 1){
      stopifnot(.is_eemlist(eem) | .is_eem(eem), is.data.frame(absorbance),
                is.numeric(cuvle))
      if (.is_eemlist(eem)) {
        res <- lapply(eem, ife_eemR, absorbance = absorbance,
                      cuvle = cuvle)
        class(res) <- class(eem)
        return(res)
      }
      if (!any(names(absorbance) == "wavelength")) {
        stop("'wavelength' variable was not found in the data frame.",
             call. = FALSE)
      }
      wl <- absorbance[["wavelength"]]
      if (!all(is_between(range(eem$em), min(wl), max(wl)))) {
        stop("absorbance wavelengths are not in the range of\n         emission wavelengths",
             call. = FALSE)
      }
      if (!all(is_between(range(eem$ex), min(wl), max(wl)))) {
        stop("absorbance wavelengths are not in the range of\n         excitation wavelengths",
             call. = FALSE)
      }
      index <- which(names(absorbance) == eem$meta_name)
      if (length(index) == 0) {
        warning("Absorbance spectrum for ", eem$sample, " was not found. Returning uncorrected EEM.",
                call. = FALSE)
        return(eem)
      }
      spectra <- absorbance[[index]]
      if (attributes(eem)$is_ife_corrected) {
        return(eem)
      }
      sf <- stats::splinefun(wl, spectra)
      ex <- sf(eem$ex)
      em <- sf(eem$em)
      total_absorbance <- sapply(ex, function(x) {
        x + em
      })/cuvle
      max_abs <- max(total_absorbance)
      if (max_abs > 1.5) {
        warning(eem$sample, ": Total absorbance is > 1.5 (Atotal = ", max_abs,
            ")\n", "A 2-fold dilution is recommended. See eemR::eem_inner_filter_effect.\n",
            sep = "")
      }
      ife_correction_factor <- 10^(0.5 * total_absorbance)

      eem$x <- eem$x * ife_correction_factor
      attr(eem, "is_ife_corrected") <- TRUE
      return(eem)
    }
    res <- ife_eemR(eemlist, abs_table, cuvle = cuvle)

    #put inner filter effect corrected eems back into augmented eemlist
    res <- lapply(1:length(eemlist), function(i){
      eemlist[[i]]$x <- res[[i]]$x
      attr(eemlist[[i]], "is_ife_corrected") <- TRUE
      return(eemlist[[i]])
    })

    #write processing to readme
    .write_readme_line("data was corrected for inner filter effects via 'ife_correct' function", "eem_ife_corrected", args)
    if(trim){
      ex_range <- ifelse(length(ex_rm)>0, paste0(range(ex_rm), collapse=" - "),"")
      em_range <- ifelse(length(em_rm)>0, paste0(range(em_rm), collapse=" - "),"")

      args <- list(excitation = ex_range, emission=em_range)

      .write_readme_line(paste0("   warning: removed the following wavelengths in EEM's to match absorbance data wavelengths\n\texcitation: ",
                         ex_range, "\n\temission: ", em_range, "\n"), "eem_ife_corrected", NULL, append=TRUE)

      }
    class(res) <- "eemlist"

    return(res)
}
