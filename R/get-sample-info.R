#' Extract components from an `eemlist` or `abslist`
#'
#' Helper function building on [eemR::eem_names()]. Extracts a specified
#' component from an `eem` or `abs` object.
#'
#' @param x An `eemlist`, `eem`, `abslist`, or `abs` object.
#' @param info The name of the component to extract.
#'   See [eemR::eem] for base `eem` component names,
#'   [abs_read()] for base `abs` component names,
#'   and [add_metadata()] for extended `eem` AND `abs` metadata fields.
#'
#' @returns if \code{x} is an \code{eemlist} or \code{abslist}:
#'
#' @return
#' If `x` is an `eemlist` or `abslist`, the returned structure depends on the
#' type of the requested component (`info`):
#TODO: check this is right
#' - **vector of length 1**: returns a vector with one value per sample
#' - **vector of length >1**: returns a data.frame (samples in columns)
#' - **matrix**: returns a list of matrices (one per sample)
#'
#' If `x` is a single `eem` or `abs` object, the corresponding component is
#' returned as-is, preserving its class and structure.
#'
#' @export
#' @md
#'
#' @examples
#' # Get sample names
#' get_sample_info(example_eems, "sample")
#' get_sample_info(example_abs, "sample")
#'
#' # Get analysis dates (metadata required)
#' eemlist <- add_metadata(metadata, example_eems)
#' get_sample_info(eemlist, "analysis_date")
#'
#' # Get DOC for the fifth sample
#' get_sample_info(eemlist[[5]], "doc_mgL")
#'
#' # Get emission wavelengths
#' get_sample_info(example_eems, "em")
#'
#' # Get absorbance matrices
#' get_sample_info(example_abs, "data")
get_sample_info <- function(x, info) {
  stopifnot(.is_eemlist(x) | .is_eem(x) | .is_abslist(x) | .is_abs(x))

  # Make sure x is an eemlist or abslist
  if (inherits(x, "eemlist") | inherits(x, "abslist")) {
    res <- lapply(x, function(y) y[[info]])

    if (all(sapply(res, is.null))) {
      stop(paste0("component '", info, "' not found in dataset"))
    }
    # if matrix, treat differently than a vector
    if (all(sapply(res, is.matrix))) {
      # Add excitation and emission wavelengths to matrix
      res_form <- res # currently just return a list of extract matrices
    }

    # if data.frame (used for absorbance data) return a data.frame of absorbance values
    if (all(sapply(res, is.matrix)) & .is_abslist(x)) {
      sample_names <- get_sample_info(x, "sample")


      # TODO We might have to check that all wavelengths are the same before merging into data.frame
      # confirm this works with test
      # convert to df (do we want a data.frame for a matrix to be consistent)
      res <- lapply(res, as.data.frame)
      res <- mapply(function(df, name) {
        colnames(df) <- c("wavelength", name)
        return(df)
      }, res, sample_names, SIMPLIFY = FALSE)
      res_form <- Reduce(
        \(df1, df2) merge(df1, df2, by = "wavelength", all.x = TRUE),
        res
      )
    }

    # if vector with multiple items
    if (all(sapply(res, is.vector)) & all(sapply(res, length) > 1)) {
      res_form <- do.call(rbind, res)
    }

    # if vector with one item
    if (all(sapply(res, length) == 1)) {
      res_form <- do.call("c", res)
    }
  }
  if (inherits(x, "eem") | inherits(x, "abs")) {
    res_form <- x[[info]]

    if (is.null(res_form)) {
      stop(paste0("component '", info, "' not found in dataset"))
    }
  }

  return(res_form)
}
