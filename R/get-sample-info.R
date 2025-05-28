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
#' see \link[eemanalyzeR]{add_metadata} for extended \code{eem} component names.
#'
#' @returns if \code{x} is an \code{eemlist} or \code{abslist}:
#'
#' if requested component (\code{info}) is a:
#'  \itemize{
#'  \item vector of length one: a vector of values where each value corresponds to the info for each sample
#'  \item vector of length greater than one: a matrix of values where each row corresponds to the info for each sample
#'  \item matrix: returns a list of matrices where each list item corresponds to the info for each sample
#'  }
#'
#' if \code{x} is an \code{eem} or \code{abs} it will return the component, maintaining class and structure.
#'
#' @export
#'
#' @examples
#' #get names
#' get_sample_info(example_eems, "sample")
#' get_sample_info(example_absorbance, "sample")
#'
#' #get analysis_date
#' eemlist <- add_metadata(metadata, example_eems)
#' get_sample_info(eemlist, "analysis_date")
#'
#' #get doc for fifth eem
#' eemlist <- add_metadata(metadata, example_eems)
#' get_sample_info(eemlist[[5]], "doc_mgL")
#'
#' #get emission wavelengths
#' get_sample_info(example_eems, "em")
#'
#' #get absorbance data
#' get_sample_info(example_absorbance, "data")

get_sample_info <- function(x, info) {
  stopifnot(.is_eemlist(x) | .is_eem(x) | .is_abslist(x) | .is_abs(x))

  if(inherits(x, "eemlist") | inherits(x, "abslist") ){
    res <- lapply(x, function(y) y[[info]])

    if(all(sapply(res, is.null))){
      stop(paste0("component '", info, "' not found in dataset"))
    }
    #if matrix, treat differently than a vector
    if(all(sapply(res, is.matrix))){
      # Add excitation and emission wavelengths to matrix
      res_form <- res #currently just return a list of extract matrices
    }

    # if data.frame (used for absorbance data) return a data.frame of absorbance values
    if(all(sapply(res, is.matrix)) & .is_abslist(x)) {
      sample_names <- get_sample_info(x, "sample")

      # TODO We might have to check that all wavelengths are the same before merging into data.frame

      #convert to df
      res <- lapply(res, as.data.frame)
      res <- mapply(function(df, name) {
        colnames(df) <- c("wavelength", name)
        return(df)
      }, res, sample_names, SIMPLIFY = FALSE)
      res_form <- Reduce(\(df1, df2) merge(df1, df2, by = "wavelength", all.x = TRUE),
      res)
    }

    #if vector with multiple items
    if(all(sapply(res, is.vector)) & all(sapply(res, length)>1)){
      res_form <- do.call(rbind, res)
    }

    #if vector with one item
    if(all(sapply(res, length)==1)){
      res_form <- do.call("c", res)
    }
  }
  if(inherits(x, "eem") | inherits(x, "abs")){
    res_form <- x[[info]]

      if(is.matrix(res_form)) {

      # Add ex and em wavelengths to matrix
      ex <- get_sample_info(x, "ex")
      em <- get_sample_info(x, "em")
      colnames(res_form) <- ex
      rownames(res_form) <- round(em, 0)

    }

    if(is.null(res_form)){stop(paste0("component '", info, "' not found in dataset"))}
  }

  return(res_form)
}


