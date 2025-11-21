#' Convert between an eem object and a data.frame
#'
#' Sometimes it is easier to subset an EEMs matrix if it is in a long format,
#' this will take an EEM matrix and turn it into a data.frame or if it is a data.frame
#' it will convert it to an `eem` object.
#'
#' @param file optional, file path to the eem data including the data file name, only used if `eem` is a data.frame
#' @param sample optional, the name of the eem sample, only used if `eem` is a data.frame
#' @param location optional, the path to the directory where the file is located, only used if `eem` is a data.frame
#' @param eem an object of class `eem` or a data.frame with three columns: ex, em, and flour
#'
#' @details
#' If you're loading in sample that have been partially processed, its recommended that you set the correct processing
#' attributes to TRUE to note that these step have already been performed. To see attributes use `attributes(eem)`, to
#' set attributes use `attr(eem, "attribute_name") <- TRUE`.
#'
#' @md
#' @returns Converts between an object of class `eem` and
#' a `data.frame` with three columns:
#'  - ex: the excitation wavelengths
#'  - em: the emission wavelengths
#'  - fluor: the fluorescence value
#' @export
#' @examples
#' flat_eem <- eem_transform(example_eems[[1]])
#' eem_obj <- eem_transform(flat_eem)
eem_transform <- function(eem, file=NULL, sample=NULL, location=NULL){
  stopifnot(.is_eem(eem) | is.data.frame(eem))

  if(.is_eem(eem)){
    df <- data.frame(ex= rep(eem$ex, each=length(eem$em)),
                     em = rep(eem$em, length(eem$ex)),
                     fluor = as.vector(eem$x))

    return(df)

  }

  if(is.data.frame(eem)){

    eem_obj <- list(file=ifelse(is.null(file), NA, file),
                    sample=ifelse(is.null(sample), NA, sample),
                    x = matrix(eem$fluor, ncol=length(unique(eem$ex)), nrow=length(unique(eem$em))),
                    ex = unique(eem$ex),
                    em = unique(eem$em),
                    location=ifelse(is.null(location), NA, location))
    class(eem_obj) <- "eem"

    #add attributes
    attributes(eem_obj) <- append(attributes(eem_obj), list("is_blank_corrected" = FALSE,
                                                            "is_scatter_corrected"= FALSE,
                                                            "is_ife_corrected"= FALSE,
                                                            "is_raman_normalized"= FALSE,
                                                            "is_doc_normalized"= FALSE,
                                                            "is_dil_corrected"= FALSE,
                                                            "sample_type" = "none"))

    return(eem_obj)
  }


}

