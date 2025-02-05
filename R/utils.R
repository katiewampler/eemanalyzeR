
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
#' eem_subset <- subset_samples(example_eems,"sample", names[1], keep=TRUE) #but use keep to keep instead
#'
#' #subset by file_name
#' eemlist <- eem_add_meta(metadata, example_eems)
#' names <- subset_samples(eemlist, "meta_name")
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
