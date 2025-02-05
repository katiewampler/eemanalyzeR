
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

#' Get unique EEM's or absorbance
#'
#' @param x an object of class \code{eemlist} or \code{abslist}
#' @param ... additional arguments passed to \link[base]{unique}

#' @rdname unique-samples
#' @name unique-samples
#' @method unique eemlist
#' @returns an object of class \code{eemlist} or \code{abslist} with samples with duplicate EEM's matrices (eem$x) or absorbance data (abs$data) removed.
#'
#' Note: sample names remain unchanged.
#' @export
#'
#' @examples
#' unique_eems <- unique(example_eems)
#' unique_abs <- unique(example_absorbance)

  unique.eemlist <- function(x, ...){
    stopifnot(.is_eemlist(x))

    flat_X <- lapply(lapply(x, `[[`, 3), as.vector)

    # Find unique matrices based on the flattened vectors
    x <- x[!duplicated(lapply(flat_X, sort))]

    class(x) <- "eemlist"
    return(x)
  }

#' @rdname unique-samples
#' @method unique abslist
#' @export
  unique.abslist <- function(x, ...){
    stopifnot(.is_abslist(x))

    flat_X <- lapply(lapply(x, `[[`, 4), as.vector)

    # Find unique matrices based on the flattened vectors
    x <- x[!duplicated(lapply(flat_X, sort))]

    class(x) <- "abslist"
    return(x)
  }


#' Extract info from eemlist or abslist
#'
#' Helper function that build upon the \link[eemR]{eem_names}
#' function. Extracts components of an \code{eemlist/eem} and \code{abslist/abs} object.
#'
#' @param x an object of class:
#' \itemize{
#' \item{\code{eemlist}}
#' \item{\code{eem}}
#' \item{\code{abslist}}
#' \item{\code{abs}}
#' }
#' @param info the name of the component within the \code{eem} or \code{abs} to extract.
#' see \link[eemR]{eem} for base \code{eem} component names and \link[eemanalyzeR]{abs_read} for base \code{abs} names.
#' see \link[eemanalyzeR]{add_meta} for extended \code{eem} component names.
#'
#' @returns returns a vector containing the info from the EEM's or absorbance.
#'
#' @export
#'
#' @examples
#' #get names
#' get_sample_info(example_eems, "sample")
#' get_sample_info(example_absorbance, "sample")
#'
#' #get analysis_date
#' eemlist <- eem_add_meta(metadata, example_eems)
#' get_sample_info(eemlist, "analysis_date")
#'
#' #get doc for fifth eem
#' eemlist <- eem_add_meta(metadata, example_eems)
#' get_sample_info(eemlist[[5]], "doc_mgL")

  get_sample_info <- function(x, info) {
    stopifnot(.is_eemlist(x) | .is_eem(x) | .is_abslist(x) | .is_abs(x))

    if(inherits(x, "eemlist") | inherits(x, "abslist") ){
      res <- unlist(lapply(x, function(y) y[[info]]))
    }
    if(inherits(x, "eem") | inherits(x, "abs")){
      res <- x[[info]]
    }
    return(res)
  }


#' Subset eemlist or abslist using components
#'
#' Helper function that build upon \link[eemR]{eem_extract} function. Used to select or remove samples based on the info extracted by
#' \link[eemanalyzeR]{get_sample_info} allowing selection of samples based on components besides just the sample name.
#'
#' @param x an object of class \code{eemlist} or \code{abslist}
#' @param sample a vector of the names or other info to use to select EEM's from \code{eemlist} or absorbance from \code{abslist}
#' @param info the name of the component within the \code{eem} or \code{abs} to extract.
#' see \link[eemR]{eem} for base \code{eem} component names and \link[eemanalyzeR]{abs_read} for base \code{abs} names.
#' see \link[eemanalyzeR]{add_meta} for extended \code{eem} component names.
#' @param keep logical. If TRUE, the specified sample will be returned. If FALSE, they will be removed.
#' @param ignore_case logical, should sample name case should be ignored (TRUE) or not (FALSE). Default is FALSE.
#' @param verbose logical determining if removed/extracted eems should be printed on screen.
#'
#' @returns
#' An object of class \code{eemlist} or \code{abslist} with the samples excluded or selected based on the 'sample' vector.
#' @export
#' @examples
#'  #subset by name
#' names <- get_sample_info(example_eems, "sample")
#' eem_subset <- subset_samples(example_eems, "sample", names[1]) #default is to remove
#' #but use keep=T to keep instead
#' eem_subset <- subset_samples(example_eems,"sample", names[1], keep=TRUE)
#'
#' #subset by file_name
#' eemlist <- eem_add_meta(metadata, example_eems)
#' names <- get_sample_info(eemlist, "meta_name")
#' eem_subset <- subset_samples(eemlist, "meta_name", names[1]) #default is to remove
#'

subset_samples <- function(x, info, sample, keep=F, ignore_case=F,
                         verbose=T){
    stopifnot(inherits(x, "eemlist") | inherits(x, "abslist"), info %in% unlist(lapply(x,names)))
    values <- get_sample_info(x, info)
    to_remove <- grepl(paste(sample, collapse = "|"), values,
                         ignore.case = ignore_case)
      x[xor(to_remove, keep)] <- NULL
      if (verbose) {
        if (all(to_remove == FALSE)) {
          cat("Nothing to remove.")
        }
        else {
          cat(ifelse(keep, "Extracted sample(s):", "Removed sample(s):"),
              values[to_remove], "\n")
        }
      }
    return(x)
}

#' Returns eemanalyzeR package version loaded
#'
#' @return text string with eemanalyzeR package version
.eemanalyzeR_ver <- function() {
  paste0("eemanalyzeR_", utils::packageVersion("eemanalyzeR"))
}

#' Generate spectral index documenation for
#'
#' @return data.frame with information documenting column lables of spectral indices spreadsheet
#'
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
#' @export
#'
#' @examples
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


# Answer validation questions yes or no
.yesorno <- function(question,
                     y_response,
                     n_response) {
  stopifnot(is.character(question) |
              is.character(y_response) |
              is.character(n_response))
  cont <- readline(paste0(question, " [y/n]"))
  if(grepl("^y$", cont, ignore.case = TRUE)) {
    cat(y_response, "\n")
    return(TRUE)
  } else if(grepl("^n$", cont, ignore.case = TRUE)){
    cat(n_response, "\n")
    return(FALSE)
  } else {
    cat("Improper response, please respond y or n", "\n")
    .yesorno(question,
             y_response,
             n_response)
  }
}



