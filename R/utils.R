#Functions to check if the objects are the right class

  #' Checks if object is an eem
  #'
  #' @param eem an object
  #' @noRd
  #' @source This function was directly pulled from \link[staRdom]
  .is_eem <- function(eem) {
    ifelse(class(eem) == "eem", TRUE, FALSE)
  }

  #' Checks if object is an eemlist
  #'
  #' @param eem an object
  #' @noRd
  #' @source This function was directly pulled from \link[staRdom]
  .is_eemlist <- function(eem) {
    ifelse(class(eem) == "eemlist", TRUE, FALSE)
  }


  #' Checks if object is an abs
  #'
  #' @param abs an object
  #' @noRd
  #' @source This function was modified from \link[staRdom]
  .is_abs <- function(abs) {
    ifelse(class(abs) == "abs", TRUE, FALSE)
  }

  #' Checks if object is an abslist
  #'
  #' @param abs an object
  #' @noRd
  #' @source This function was modified from \link[staRdom]
  .is_abslist <- function(abs) {
    ifelse(class(abs) == "abslist", TRUE, FALSE)
  }


#' Returns eemanalyzeR package version loaded
#'
#' @return text string with eemanalyzeR package version
#' @noRd
.eemanalyzeR_ver <- function() {
  paste0("eemanalyzeR_", utils::packageVersion("eemanalyzeR"))
}

#' Generate spectral index documenation for
#'
#' @return data.frame with information documenting column lables of spectral indices spreadsheet
#' @noRd
.document_indices <- function() {

  eemanalyzeR_version <- .eemanalyzeR_ver()

  documentation <- data.frame(c(paste0("Peaks extracted using ", eemanalyzeR_version, " package in R."),
                                "For peak definitions see 'eem_coble_peaks2' and 'abs_parm' functions in the eemanalyzeR package.",
                                "The package can be downloaded from https://github.com/katiewampler/eemanalyzeR",
                                "",
                                "Sheet fluor_indices_DOC contains fluorsecence indices normalized by DOC concentration",
                                "Sheet fluor_indices contains raw fluorsecence indices",
                                "sheet abs_indices contains absorbance indices",
                                "",
                                "Fluorescence Indices",
                                "Coble peaks are based on Coble et al. 2014 and are defined as follows:",
                                "Peak B (pB): ex = 270:280 nm, em = 300:320 nm, Tyrosine-like",
                                "Peak T (pT): ex = 270:280 nm, em = 320:350 nm, Tryptophan-like",
                                "Peak A (pA): ex = 250:260 nm, em = 380:480 nm, Humic-like.",
                                "Peak M (pM): ex = 310:320 nm, em = 380:420 nm, Marine humic-like",
                                "Peak C (pC): ex = 330:350 nm, em = 420:480 nm, Humic-like",
                                "Peak D (pD): ex = 390 nm, em = 509 nm, Soil fulvic acid",
                                "Peak E (pE): ex = 455 nm, em = 521 nm, Soil fulvic acid",
                                "Peak N (pN): ex = 280 nm, em = 370 nm, Plankton derived",
                                "Given that peaks A, B, C, M, and T are not defined at fixed excitation and emission wavelength, the maximum fluorescence value in the region is extracted.",
                                "",
                                "Additional fluorescence indices are based on Hansen et al. 2016.",
                                "Measurements are defined as follows:",
                                "rAT: The ratio of peak A to peak T, indication of the amount of humic like (recalcitrant) to fresh (liable) DOM.",
                                "rCA: The ratio of peak C to peak A, indication of the amount of humic like to fumic like DOM.",
                                "rCM: The ratio of peak C to peak M, indication of the amount of diagenetically altered (blueshifted) DOM.",
                                "rCT: The ratio of peak C to peak T, indication of the amount of humic like (recalcitrant) to fresh (liable) DOM.",
                                "Fluorescence Index (FI): Ratio of fluorescence at ex = 370 nm, em = 470 nm to em = 520 nm.",
                                "Identifies the relative contributions of terrestrial to microbial DOM sources.",
                                "",
                                "Humification Index (HIX): ex = 254 nm, em =sum(435:480 divided by em =sum(435:480, sum(300:345.",
                                "An indication of humic substances or extent of humification. Higher values indicate an higher degree of humification.",
                                "",
                                "Humification Index (HIX_ohno): ex = 254 nm, em =sum(435:480 divided by em =sum(435:480, sum(300:345. HIX proposed by Ohno (2002), both versions of HIX are used throughout the literature. Ohno is better when samples have higher absorbance because it accounts for inner filter effects better.",
                                "Freshness Index (beta/alpha fresh): ex = 310 nm, ratio of em = 380 nm to max in em = 420:435 nm.",
                                "An indication of recently produced DOM, higher values indicate more recently produced DOM.",
                                "",
                                "Relative Fluorescence Efficiency (RFE): Ratio of fluorescence at ex = 370 nm, em = 460 nm to",
                                "absorbance at 370 nm. An indicator of the relative amount of algal to non-algal DOM.",
                                "",
                                "Biological Index (BIX): ex = 310 nm, ratio of em = 380 nm to em = 430 nm.",
                                "An indicator of autotrophic productivity, values above 1 indicate recently produced",
                                "autochthonous DOM.",
                                "",
                                "Absorbance indices",
                                "Absorbance indices based on Hansen et al. 2016. Measurements are defined as follows:",
                                "",
                                "SUVA254, SUVA280, SUVA350, SUVA370: SUVA at 254, 280, 350, and 370 nm.",
                                "Units of L mgC^-1 m^-1.",
                                "Typically higher values are associated with greater aromatic content.",
                                "",
                                "SVA412, SVA440, SVA480, SVA510,",
                                "SVA532, SVA555: SVA at 412, 440, 480, 510, 532, 555 nm.",
                                "Units of L mgC^-1 m^-1.",
                                "Typically higher values are associated with greater aromatic content.",
                                "",
                                "S275_295: Spectral slope between 275 to 295 nm.",
                                "",
                                "S350_400: Spectral slope between 350 to 400 nm.",
                                "",
                                "Spectral slopes are found with a nonlinear fit of an exponential function to",
                                "the absorption spectrum, typically higher  values are associated with",
                                "lower molecular weight materials and/or lower aromaticity.",
                                "",
                                "SR: Spectral slope S275_295 divided by spectral slope S350_400, negatively correlated to DOM molecular weight",
                                "and generally increases on irradiation."))
  return(documentation)
}

#' Write a line of text to the process file that tracks processing tracking
#'
#' @param text Line of text to write to the process file
#' @param overwrite if FALSE (the default) it appends the line of text to the process file. If TRUE, creates a new process file (for fresh processing of data).
#'
#' @return invisible copy of text written to process file
#'@noRd
.write_processing_tracking <- function(text,
                                       overwrite = FALSE) {

  process_file <- get_process_file()
  # Check that we want to do this otherwise exit the function
  stopifnot(is.character(text) | !is.null(process_file))

  #create text file to track processing changes
  if (overwrite) {
    # If the file should be overwritten (new processing run), create a new file
    file.create(process_file)
    line_write <- paste0("PROCESSING STEPS ON ", round(Sys.time(), "secs"), " using ", .eemanalyzeR_ver())
    write(line_write,
          file = process_file)
  } else {
    # This case we just append a new line to the end of the file with the text
    line_write <- paste(round(Sys.time(),
                              "secs"),
                        text,
                        sep = " - ")
    write(line_write,
          file = process_file,
          append = TRUE)
  }

  invisible(line_write)

}


#' Answer validation questions yes or no
#'
#' @importFrom rlang is_interactive
#' @noRd
.yesorno <- function(question,
                     y_response,
                     n_response) {
  # Return TRUE (ie "yes") if run non-interactively (tests, batch processing)
  if (!rlang::is_interactive()) return(TRUE)
  stopifnot(  is.character(question) |
              is.character(y_response) |
              is.character(n_response))
  cont <- readline(paste0(question, " [y/n]: "))
  if(grepl("^y$", cont, ignore.case = TRUE)) {
    message(y_response, "\n")
    return(TRUE)
  } else if(grepl("^n$", cont, ignore.case = TRUE)){
    message(n_response, "\n")
    return(FALSE)
  } else {
    warning("Improper response, please respond 'y' or 'n'", "\n")
    .yesorno(question,
             y_response,
             n_response)
  }
}


#' Checks if the eems or absorbance has had metadata added
#' @noRd
.meta_added <- function(x){
  stopifnot(class(x) %in% c("eemlist", "eem", "abs", "abslist"))

  if(inherits(x, c("eem","abs"))){
    items <- names(x)
    augment_names <- c("meta_name","dilution","analysis_date", "description","doc_mgL","notes")
    augmented <- all(augment_names %in% items)
   }else{
    augmented <- unlist(lapply(x, .meta_added))
   }
  return(augmented)
}

#' Checks if the eems or absorbance has had blank added
#' @noRd
.blk_added <- function(x){
  stopifnot(class(x) %in% c("eemlist", "eem", "abs", "abslist"))

  if(inherits(x, c("eem","abs"))){
    items <- names(x)
    augment_names <- c("blk_file", "blk_x")
    augmented <- all(augment_names %in% items)
  }else{
    augmented <- unlist(lapply(x, .blk_added))
  }
  return(augmented)

}


#' Check if two eem matrices are equal
#'
#' @param x1 a matrix "x" from an eem
#' @param x2 a matrix "x" from an eem
#'
#' @noRd
#'
.eem_equal <- function(x1, x2){
  x1_long <- as.vector(x1)
  x2_long <- as.vector(x2)

  equal <- all.equal(x1_long, x2_long)
  return(equal)
}

#' Normalize an eem or eemlist based on a normalization factor
#'
#' Useful for raman normalization or normalizing to a max of one for blank comparisons.
#'
#' @param eem the \code{eem} or \code{eemlist} to normalize
#' @param factor the normalization factor, if NULL it will normalize to the maximum value for each eem
#'
#' @return an \code{eem} or \code{eemlist} where \code{x} has been normalized
#' @export
#'
#' @examples
#' eem_normal <- eem_normalize(example_eems[1])
#' eems_normal <- eem_normalize(example_eems)
#'
eem_normalize <- function(eem, factor=NULL){

  if(.is_eemlist(eem)){
    eem <- lapply(eem, eem_normalize, factor)
    class(eem) <- "eemlist"
  }else{
    if(is.null(factor)){
      factor <- max(eem$x, na.rm = T)
    }

    eem$x <- eem$x / factor
    class(eem) <- "eem"
  }

  return(eem)
}

#' Subtract one eem from another
#'
#' Useful for performing blank subtraction
#'
#' @param eem1 the eem that will be returned
#' @param eem2 the eem that will be used for subtraction
#'
#' @importFrom staRdom eem_extend2largest
#' @importFrom staRdom eem_red2smallest
#' @return an \code{eem}
#' @noRd
#' @examples
#' eem_sub <- .eem_subtract(example_eems[[1]], longterm_blank)

.eem_subtract <- function(eem1, eem2){
  #scale subtraction eem to returned eem
  if(any(dim(eem2$x) > dim(eem1$x))){
    eem2 <- staRdom::eem_red2smallest(list(eem1, eem2))[[2]]
  }

  if(any(dim(eem2$x) < dim(eem1$x)) | length(setdiff(eem2$em, eem1$em)) > 0 | length(setdiff(eem2$ex, eem1$ex)) > 0){
    eem2 <- staRdom::eem_extend2largest(list(eem1, eem2), interpolation = T)[[2]]
  }

  #ensure wavelengths match eem1
  if(length(setdiff(eem2$em, eem1$em)) > 0 | length(setdiff(eem2$ex, eem1$ex)) > 0){
    em_rm <- setdiff(eem2$em, eem1$em)
    ex_rm <- setdiff(eem2$ex, eem1$ex)

    if(length(em_rm) > 0){
      eem2 <- staRdom::eem_exclude(list(eem2), exclude=list("em"=em_rm))[[1]]
    }

    if(length(ex_rm) > 0){
      eem2 <- staRdom::eem_exclude(list(eem2), exclude=list("ex"=ex_rm))[[1]]
    }
  }

  #check dimensions are equal
  if(any(dim(eem2$x) != dim(eem1$x))){
    stop("unable to make eem2 dimensions match eem1")
  }

  #subtract
  eem1$x <- eem1$x - eem2$x
  return(eem1)
}
