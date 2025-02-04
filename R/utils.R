
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


#' Get the names of abs or abslist objects
#'
#' @param abs an object of class \code{abs} or \code{abslist}
#'
#' @returns A character vector containing the names of the absorbance data
#' @export
#'
#' @examples
#' abs <- abs_dir_read(system.file("extdata", package = "eemanalyzeR"))
#' names <- abs_names(abs)
#'
#' abs_files <- list.files(system.file("extdata", package = "eemanalyzeR"),
#' full.names=TRUE, pattern="ABS")
#' abs <- abs_read(abs_files[1])
#' name <- abs_names(abs)

  abs_names <- function(abs){
    stopifnot(.is_abslist(abs) | .is_abs(abs))
    if (.is_abslist(abs)) {
      res <- unlist(lapply(abs, abs_names))
      return(res)
    }
    return(abs$sample)
  }

#' Get unique EEM's
#'
#' @param eemlist an object of class \code{eemlist}
#'
#' @returns an object of class \code{eemlist} with samples with duplicate EEM's matrices (eem$x) removed.
#'
#' Note: sample names remain unchanged.
#' @export
#'
#' @examples
#' unique_eems <- eem_unique(example_blanks)
  eem_unique <- function(eemlist){
    stopifnot(class(eemlist) == "eemlist")

    flat_X <- lapply(lapply(eemlist, `[[`, 3), as.vector)

    # Find unique matrices based on the flattened vectors
    eemlist <- eemlist[!duplicated(lapply(flat_X, sort))]

    class(eemlist) <- "eemlist"
    return(eemlist)
  }


#' Extract blanks from eemlist
#'
#' Using a regular expression, the \code{eemlist} will be cut to include either
#' only the blank samples (\code{eem_get_blank}) or remove all blank samples (\code{eem_rm_blank}).
#' This function is similar to the \link[eemR]{eem_remove_blank} function, except it is more
#' flexible to include different patterns for the blank.
#'
#' @param eemlist an object of class \code{eemlist}
#' @param pattern a character string containing a \code{\link[base]{regular expression}}
#' used to specify the sample names of the blanks.
#' @param info the name of the component within the \code{eem} to check for the pattern. default is 'sample'

#' @note see \link[eemR]{eem} for base \code{eem} component names and \link[eemanalyzeR]{add_meta}
#' for extended \code{eem} component names.
#'
#' @returns an object of class \code{eemlist} either with only the blanks (\code{eem_get_blank})
#' or only the samples (\code{eem_rm_blank})
#' @export
#'
#' @rdname extract-blanks
#' @name extract-blanks
#'
#' @examples
#' blanks <- eem_get_blank(example_eems, pattern = "BEM")
#' samples <- eem_rm_blank(example_eems, pattern = "BEM")
 eem_get_blank <- function(eemlist, pattern, info="sample"){

   blank_names <- grep(pattern, eem_get_info(eemlist, info), value = T)
   eemlist <- eem_select(eemlist, info, blank_names, keep = TRUE, ignore_case = TRUE,verbose = FALSE)
   class(eemlist) <- "eemlist"
   return(eemlist)
 }

#' @rdname extract-blanks
#' @export

 eem_rm_blank <- function(eemlist, pattern, info="sample"){
   blank_names <- grep(pattern, eem_get_info(eemlist, info), value = T)
   eemlist <- eem_select(eemlist, info, blank_names, keep = FALSE, ignore_case = TRUE,verbose = FALSE)
   class(eemlist) <- "eemlist"
   return(eemlist)
 }

#' Subset eemlist using components
#'
#' These are helper functions that build upon the \link[eemR]{eem_names} and \link[eemR]{eem_extract}
#' functions. \code{eem_get_info} will extract components of an \code{eem} object. While
#' \code{eem_select} will select or remove samples based on the info extracted by \code{eem_get_info}.
#' These functions allow selection based on components besides just the sample name.
#'
#' @param eem an object of class \code{eem} of \code{eemlist}
#' @param sample a vector of the names or other info to use to select EEM's from \code{eemlist}
#' @param info the name of the component within the \code{eem} to extract.
#' see \link[eemR]{eem} for base \code{eem} component names and \link[eemanalyzeR]{add_meta}
#' for extended \code{eem} component names.
#' @param keep logical. If TRUE, the specified sample will be returned. If FALSE, they will be removed.
#' @param ignore_case logical, should sample name case should be ignored (TRUE) or not (FALSE). Default is FALSE.
#' @param verbose logical determining if removed/extracted eems should be printed on screen.
#'
#' @rdname subset-eems
#' @name subset-eems
#' @returns \code{eem_get_info} returns a vector containing the info from the EEM's.
#'
#' \code{eem_select} returns an object of class \code{eemlist} with the samples excluded or selected based on the 'sample' vector.
#' @export
#'
#' @examples
#' #get names
#' eem_get_info(example_eems, "sample")
#'
#' #get analysis_date
#' eemlist <- eem_add_meta(metadata, example_eems)
#' eem_get_info(eemlist, "analysis_date")
#'
#' #get doc
#' eemlist <- eem_add_meta(metadata, example_eems)
#' eem_get_info(eemlist, "doc_mgL")
#'
#' #subset by name
#' names <- eem_get_info(example_eems, "sample")
#' eem_subset <- eem_select(example_eems, "sample", names[1]) #default is to remove
#' eem_subset <- eem_select(example_eems,"sample", names[1], keep=T) #but use keep to keep instead
#'
#' #subset by file_name
#' eemlist <- eem_add_meta(metadata, example_eems)
#' names <- eem_get_info(eemlist, "file_name")
#' eem_subset <- eem_select(eemlist, "file_name", names[1]) #default is to remove
#'

  eem_get_info <- function(eem, info){
   stopifnot(.is_eemlist(eem) | .is_eem(eem))
   if (.is_eemlist(eem)) {
     res <- unlist(lapply(eem, function(x) x[[info]]))
   }else{
     res <- eem[[info]]
   }
   return(res)
  }

#' @rdname subset-eems
#' @export

  eem_select <- function(eem, info, sample, keep=F, ignore_case=F,
                         verbose=T){
    stopifnot(class(eem) == "eemlist", info %in% unlist(lapply(eem,names)))
    values <- eem_get_info(eem, info)
    to_remove <- grepl(paste(sample, collapse = "|"), values,
                         ignore.case = ignore_case)
      eem[xor(to_remove, keep)] <- NULL
      if (verbose) {
        if (all(to_remove == FALSE)) {
          cat("Nothing to remove.")
        }
        else {
          cat(ifelse(keep, "Extracted sample(s):", "Removed sample(s):"),
              values[to_remove], "\n")
        }
      }
    return(eem)
  }


