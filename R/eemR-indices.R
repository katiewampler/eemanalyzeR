#' eemR and staRdom methods for fluorescence and absorbance indices
#'
#' Calculates commonly used absorbance and fluorescence optical indices from `eemlist` and `abslist` objects
#' using functions from the [eemR](https://CRAN.R-project.org/package=eemR) and
#' [staRdom](https://CRAN.R-project.org/package=staRdom) packages. Can incorporate sample metadata if provided.
#'
#' @inheritParams eemanalyzeR_indices
#' @return A list with two elements:
#'
#' - **eem_index**: a `data.frame` of all fluorescence indices. Each row corresponds to a single index for a sample.
#' - **abs_index**: a `data.frame` of all absorbance indices. Each row corresponds to a single index for a sample.
#'
#' Each `data.frame` contains the following columns:
#'
#' - **sample_name**: name of the sample from the EEM or absorbance list
#' - **meta_name**: sample name from metadata if provided; otherwise same as `sample_name`
#' - **index**: name of the index
#' - **value**: calculated value of the index
#'
#' @details
#' **Absorbance indices** (a254, a300, E2_E3, E4_E6, S275_295, S350_400, S300_700, SR)
#' are calculated using [staRdom::abs_parms()].
#'
#' **Fluorescence indices** are calculated using the following [eemR] functions:
#'
#' * Coble peaks (b, t, a, m, c): [eemR::eem_coble_peaks()]
#' * fi (fluorescence index): [eemR::eem_fluorescence_index()]
#' * hix (humification index): [eemR::eem_humification_index()] (use `scale = TRUE` for `hix_scaled`)
#' * bix (biological index): [eemR::eem_biological_index()]
#'
#' @examples
#' indices <- eemR_indices(
#'   eemlist = example_processed_eems,
#'   abslist = example_processed_abs,
#'   qaqc_dir = system.file("extdata", package = "eemanalyzeR")
#' )
#'
#' # View fluorescence indices
#' head(indices$eem_index)
#'
#' # View absorbance indices
#' head(indices$abs_index)
#'
#' @export
#' @md
eemR_indices <- function(eemlist, abslist, cuvle = 1, qaqc_dir = NULL) {
  stopifnot(.is_eemlist(eemlist), .is_abslist(abslist), is.numeric(cuvle), all(sapply(eemlist, attr, "is_doc_normalized")) == FALSE)

  # get mdl data
  mdls <- .check_mdl_file(qaqc_dir)
  eem_mdl <- mdls$eem_mdl
  abs_mdl <- mdls$abs_mdl

  # get fluorescence peaks
  # define wavelengths for peaks and metrics to check if there are missing wavelengths
  # format: index = list(excitation wavelengths, emission wavelengths)
  peaks <- list(
    b = list(ex = 275, em = 310),
    t = list(ex = 275, em = 340),
    a = list(ex = 260, em = 380:460),
    m = list(ex = 312, em = 380:420),
    c = list(ex = 350, em = 420:480),
    fi = list(ex = 370, em = c(450, 500)),
    hix = list(ex = 254, em = c(300:345, 435:480)),
    hix_scaled = list(ex = 254, em = c(300:345, 435:480)),
    bix = list(ex = 310, em = c(380, 430))
  )

  split_groups <- function(em) {
    # Sort and remove duplicates
    em <- sort(unique(em))
    # Find breaks where the gap > 1
    breaks <- cumsum(c(TRUE, diff(em) != 1))
    # Split based on those breaks
    split(em, breaks)
  }

  # get metrics using eemR functions
  # get fluorescence indices
  eem_index <- lapply(eemlist, function(eem) {
    suppressWarnings(vals <- c(eemR::eem_coble_peaks(eem)[-1],
      eemR::eem_fluorescence_index(eem)[2],
      eemR::eem_humification_index(eem, scale = FALSE)[2],
      hix_scaled = unname(eemR::eem_humification_index(eem, scale = TRUE)[2]),
      eemR::eem_biological_index(eem)[2]
    ))
  })
  eem_index <- purrr::list_transpose(eem_index) # convert to better output

  # flag and format
  eem_index <- lapply(names(peaks), function(index_name) {
    index <- peaks[[index_name]]

    # get values
    vals <- eem_index[[index_name]]

    # get flags
    missflags <- ifelse(is.na(vals) | is.nan(vals), "DATA01", NA)
    mdlflags <- sapply(split_groups(index$em), function(x) {
      check_eem_mdl(eemlist, eem_mdl, index$ex, x)
    })
    if (ncol(mdlflags) > 1) {
      mdlflags <- .combine_flags(mdlflags[, 1], mdlflags[, 2], mdl = TRUE)
    } else {
      mdlflags <- as.vector(mdlflags)
    }
    flags <- .combine_flags(missflags, mdlflags)

    # add sample names and make into data.frame (get index name)
    res <- format_index(eemlist, index_name, vals, flags)

    # return res
    return(res)
  }) %>% dplyr::bind_rows()

  # absorbance peaks
  abslist <- abs_interp(abslist)
  raw_abs <- get_sample_info(abslist, "data")
  raw_abs <- as.data.frame(raw_abs)
  abs_vals <- staRdom::abs_parms(raw_abs, cuvle = 1, unit = "absorbance", cores = 1)
  abs_vals$SR[is.infinite(abs_vals$SR) | is.na(abs_vals$SR)] <- "DATA04"
  abs_vals <- as.list(abs_vals[, -1])

  # specify absorbance wavelengths to check if there's missing data
  # format: index = wavelengths in metric
  abs_wl <- list(
    a254 = 254,
    a300 = 300,
    E2_E3 = c(250, 365),
    E4_E6 = c(465, 665),
    S275_295 = 275:295,
    S350_400 = 350:400,
    S300_700 = 300:700,
    SR = c(275:295, 350:400)
  )

  abs_index <- lapply(names(abs_wl), function(index_name) {
    index <- abs_wl[[index_name]]

    # get values
    vals <- abs_vals[[index_name]]

    # get flags
    missflags <- flag_missing(abslist, wl = index)
    mdlflags <- check_abs_mdl(abslist, mdl = abs_mdl, wl = index)
    mdlflags <- sapply(split_groups(index), function(x) {
      check_abs_mdl(abslist, abs_mdl, wl = x)
    })
    if (ncol(mdlflags) > 1) {
      mdlflags <- .combine_flags(mdlflags[, 1], mdlflags[, 2], mdl = TRUE)
    } else {
      mdlflags <- as.vector(mdlflags)
    }
    flags <- .combine_flags(missflags, mdlflags)

    # add sample names and make into data.frame (get index name)
    res <- format_index(abslist, index_name, vals, flags)

    # return res
    return(res)
  }) %>% dplyr::bind_rows()


  # return indices
  index <- list(abs_index = abs_index, eem_index = eem_index)
  return(index)
}
