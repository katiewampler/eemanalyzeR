# MAJOR TODO - clean up utils across the entire package!

#Functions to check if the objects are the right class

  #' Checks if object is an eem
  #'
  #' @param eem an object
  #' @noRd
  #' @source This function was directly pulled from \link[staRdom]
  .is_eem <- function(eem) {
    ifelse(inherits(eem, "eem"), TRUE, FALSE)
  }

  #' Checks if object is an eemlist
  #'
  #' @param eem an object
  #' @noRd
  #' @source This function was directly pulled from \link[staRdom]
  .is_eemlist <- function(eem) {
    ifelse(inherits(eem, "eemlist"), TRUE, FALSE)
  }

  #' Checks if object is an abs
  #'
  #' @param abs an object
  #' @noRd
  #' @source This function was modified from \link[staRdom]
  .is_abs <- function(abs) {
    ifelse(inherits(abs, "abs"), TRUE, FALSE)
  }

  #' Checks if object is an abslist
  #'
  #' @param abs an object
  #' @noRd
  #' @source This function was modified from \link[staRdom]
  .is_abslist <- function(abs) {
    ifelse(inherits(abs, "abslist"), TRUE, FALSE)
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

# Overload the bracket operator for eemlist subsetting
#' Subsetting using `[` for eemlist
#'
#' @param eemlist the eemlist to subset
#' @param i the index for subsetting
#'
#' @export
#' @S3method `[` eemlist
#'
`[.eemlist` <- function(eemlist, i) {
  sublist <- NextMethod()
  structure(sublist, class = "eemlist")
}


# Overload the bracket operator for abslist subsetting
# we want to always return an abslist

#'Subsetting using `[` for an abslist
#' 
#' @param abslist the abslist to subset
#' @param i the index for subsetting
#' 
#' @export
#' @S3method `[` abslist
#' 
`[.abslist` <- function(abslist, i) {
  sublist <- NextMethod()
  structure(sublist, class = "abslist")
}
