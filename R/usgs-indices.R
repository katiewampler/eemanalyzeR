#' USGS methods for fluorescence and absorbance indices
#'
#' Calculates commonly used absorbance and fluorescence optical indices from `eemlist` and `abslist`.
#' These indices are those commonly used by the U.S. Geological Survey.
#' For detailed descriptions and references, see Hansen et al. 2018 (Tables 1 and 8).
#'
#' @inheritParams eemanalyzeR_indices
#'
#' @note
#' - If absorbance is not at a 1 nm interval, it will be interpolated using [zoo::na.approx()], which fills in missing values using linear interpolation.
#' - If EEM data is not at a 1 nm interval, fluorescence will be interpolated using [pracma::interp2()].
#'
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
#' @export
#' @md
#'
#' @source
#' Hansen, A. M., Fleck, J., Kraus, T. E. C., Downing, B. D., von Dessonneck, T., & Bergamaschi, B. (2018).
#' *Procedures for using the Horiba Scientific Aqualog® fluorometer to measure absorbance
#' and fluorescence from dissolved organic matter* (USGS Numbered Series No. 2018-1096).
#' U.S. Geological Survey.
#' <doi:10.3133/ofr20181096>

#' @examples
#' indices <- usgs_indices(
#'   example_processed_eems,
#'   example_processed_abs,
#'   qaqc_dir = system.file("extdata", package = "eemanalyzeR")
#' )
usgs_indices <- function(eemlist, abslist, cuvle = 1, qaqc_dir = NULL) {
  stopifnot(.is_eemlist(eemlist), .is_abslist(abslist), is.numeric(cuvle), all(sapply(eemlist, attr, "is_doc_normalized")) == FALSE)

  # get mdl data
  mdls <- .check_mdl_file(qaqc_dir)
  eem_mdl <- mdls$eem_mdl
  abs_mdl <- mdls$abs_mdl

  # get fluorescence peaks
  # define wavelengths for peaks and metrics to check if there are missing wavelengths
  # format: index = list(excitation wavelengths, emission wavelengths)
  peaks <- list(
    pA_32304 = list(ex = 260, em = 450),
    pB_32305 = list(ex = 275, em = 304),
    pC_52901 = list(ex = 340, em = 440),
    pD_32307 = list(ex = 390, em = 510),
    FDOM_52902 = list(ex = 370, em = 460),
    pM_32309 = list(ex = 300, em = 390),
    pN_32310 = list(ex = 280, em = 370),
    pT_32311 = list(ex = 275, em = 340),
    FI_32312 = list(ex = 370, em = c(470, 520)),
    HIX_32313 = list(ex = 254, em = c(300:345, 435:480))
  )


  # get coble peaks and ratios
  coble <- lapply(names(peaks[1:8]), function(index_name) {
    index <- peaks[[index_name]]

    # get values
    vals <- get_fluorescence(eemlist, index$ex, index$em, stat = "max")

    # get flags
    missflags <- flag_missing(eemlist, ex = index$ex, em = index$em, all = FALSE)
    mdlflags <- check_eem_mdl(eemlist, eem_mdl, index$ex, index$em)
    flags <- .combine_flags(missflags, mdlflags)

    # add sample names and make into data.frame (get index name)
    res <- format_index(eemlist, index_name, vals, flags)

    # return res
    return(res)
  }) %>% dplyr::bind_rows()

  # get FI
  index <- "FI_32312"
  vals <- get_ratios(get_fluorescence(eemlist, 370, 470), get_fluorescence(eemlist, 370, 520))
  missflags <- flag_missing(eemlist, ex = peaks[[index]]$ex, em = peaks[[index]]$ex, all = FALSE)
  mdlflags <- .combine_flags(check_eem_mdl(eemlist, eem_mdl, 370, 470), check_eem_mdl(eemlist, eem_mdl, 370, 520), mdl = TRUE)
  flags <- .combine_flags(missflags, mdlflags)
  FI <- format_index(eemlist, index, vals, flags)

  # get HIX
  index <- "HIX_32313"
  low <- get_fluorescence(eemlist, 254, 300:345, stat = "sum")
  high <- get_fluorescence(eemlist, 254, 435:480, stat = "sum")
  vals <- get_ratios(high, low)
  missflags <- flag_missing(eemlist, ex = peaks[[index]]$ex, em = peaks[[index]]$ex, all = FALSE)
  mdlflags <- .combine_flags(check_eem_mdl(eemlist, eem_mdl, 254, 300:345), check_eem_mdl(eemlist, eem_mdl, 254, 435:480), mdl = TRUE)
  flags <- .combine_flags(missflags, mdlflags)
  HIX <- format_index(eemlist, index, vals, flags)

  # merge indices together
  eem_index <- do.call(rbind, list(coble, FI, HIX))

  # absorbance peaks
  # specify absorbance wavelengths to check if there's missing data
  # format: index = wavelengths in metric
  abs_wl <- list(
    SUVA254_63162 = 254,
    A254_50624 = 254,
    A280_32296 = 280,
    A370_32297 = 370,
    A412_32298 = 412,
    A440_32299 = 440,
    S275_295_32300 = 275:295,
    S290_350_32301 = 290:350,
    S350_400_32302 = 350:400,
    S412_600_32331 = 412:600
  )

  # get SUVA 254
  index <- "SUVA254_63162"
  vals <- get_absorbance(abslist, wl = 254, suva = TRUE)
  missflags <- flag_missing(abslist, wl = abs_wl[[index]])
  mdlflags <- check_abs_mdl(abslist, mdl = abs_mdl, wl = 254)
  flags <- .combine_flags(missflags, mdlflags)
  suva254 <- format_index(abslist, index, vals, flags)

  # get values at specific wavelengths
  abs_data <- lapply(names(abs_wl[2:6]), function(index_name) {
    index <- abs_wl[[index_name]]

    # get values
    vals <- get_absorbance(abslist, wl = index, suva = FALSE)

    # get flags
    missflags <- flag_missing(abslist, wl = index)
    mdlflags <- check_abs_mdl(abslist, mdl = abs_mdl, wl = index)
    flags <- .combine_flags(missflags, mdlflags)

    # add sample names and make into data.frame (get index name)
    res <- format_index(abslist, index_name, vals, flags)

    # return res
    return(res)
  }) %>% dplyr::bind_rows()

  # get spectral slopes
  abs_slopes <- lapply(names(abs_wl[7:10]), function(index_name) {
    index <- abs_wl[[index_name]]
    start <- min(index)
    end <- max(index)

    # get values
    vals <- get_abs_slope(abslist, c(start, end))

    # get flags
    missflags <- flag_missing(abslist, wl = index)
    mdlflags <- check_abs_mdl(abslist, mdl = abs_mdl, wl = index)
    flags <- .combine_flags(missflags, mdlflags)

    # add sample names and make into data.frame (get index name)
    res <- format_index(abslist, index_name, vals, flags)

    # return res
    return(res)
  }) %>% dplyr::bind_rows()

  # merge indices together
  abs_index <- do.call(rbind, list(suva254, abs_data, abs_slopes))

  # return indices
  index <- list(abs_index = abs_index, eem_index = eem_index)
  return(index)
}
