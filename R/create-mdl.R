#' Calculate method detection limits (MDL)
#'
#' The method detection limit (MDL) is the minimum signal at which we can confidently
#' distinguish a measurement from zero and from analytical blanks. Here, the MDL is
#' calculated using the approach proposed by Hansen et al. (2018):
#' **MDL = mean(long-term blank) + 3 × SD(long-term blank)**.
#'
#' @param dir Path to a folder containing long-term EEMs and/or absorbance files.
#'   All files in this directory will be loaded.
#' @param meta_name Name of the metadata file. Optional if the metadata file is the
#'   only `.xlsx` or `.csv` file in `dir`. If not specified, the function attempts to find
#'   a single metadata file and errors if multiple files are present.
#' @param sheet Name of the sheet containing metadata (only required if metadata
#'   is not on the first sheet).
#' @param iblank Optional. A character string containing a
#'   [base::regular expression] used to identify instrument blanks.
#' @param type Which MDL to calculate: either "eem" or "abs".
#' @param recursive Logical. Should the function recursively search directories?
#' @param qaqc_dir Directory in which to save the MDL `.rds` file.
#'   Default: a user-specific data directory via [rappdirs::user_data_dir()].
#'   If `FALSE`, the function returns the MDL object instead of saving it.
#'
#' @returns
#' - If `qaqc_dir = FALSE`: returns an `eem` or `abs` object containing MDL values.
#' - Otherwise: saves an `.rds` file containing the MDL object and invisibly returns the file path.
#'
#' @md
#' @export
#'
#' @details
#' To calculate the MDL, you need:
#'
#' - A directory containing analytical blanks and instrument blanks
#'   (fewer than 20 blanks triggers a warning).
#'   *Note: sample names must be unique.*
#'
#' - A metadata file for the blanks containing, at minimum, integration time
#'   and Raman area, formatted as a metadata file described in [eemanalyzeR::metadata].
#'
#' @source
#' Hansen, A. M., Fleck, J., Kraus, T. E. C., Downing, B. D., von Dessonneck, T., & Bergamaschi, B. (2018).
#' *Procedures for using the Horiba Scientific Aqualog® fluorometer to measure absorbance
#' and fluorescence from dissolved organic matter* (USGS Numbered Series No. 2018-1096).
#' U.S. Geological Survey.
#' <https://doi.org/10.3133/ofr20181096>
#'
#' @examples
#' eem_mdl <- create_mdl(
#'   file.path(system.file("extdata", package = "eemanalyzeR"), "long-term-blanks"),
#'   type = "eem",
#'   qaqc_dir = FALSE
#' )
#'
#' plot(eem_mdl)
create_mdl <- function(dir, meta_name = NULL, sheet = NULL, iblank = "BEM",
                       type = "eem", recursive = FALSE, qaqc_dir = NULL) {

  stopifnot(type %in% c("eem", "abs"), dir.exists(dir))

  # set up file structure for saving mdl data
  if (is.null(qaqc_dir)) {
    qaqc_dir <- file.path(rappdirs::user_data_dir(appname = "eemanalyzeR"), "qaqc-stds")
  }
  if (qaqc_dir != FALSE) {
    dir.create(qaqc_dir, showWarnings = FALSE, recursive = TRUE)
  }

  # get metadata
  if (!is.null(meta_name)) {
    input <- file.path(dir, meta_name)
  } else {
    input <- dir
  }
  blank_meta <- meta_read(input, sheet = sheet)

  # get all blanks in directory with instrument blanks
  if (type == "eem") {
    blank <- eem_dir_read(dir, recursive = recursive)
  }
  if (type == "abs") {
    blank <- abs_dir_read(dir, recursive = recursive)
  }

  # check number of samples
  n_samps <- length(blank) / 2
  if (n_samps < 20) {
    warning("Calculating MDL based on less than 20 samples, MDL may be unreliable")
  }

  # add metadata
  blank <- add_metadata(blank_meta, blank, sample_type_regex = list(
    iblank_pattern = iblank,
    sblank_pattern = "Blank|blank",
    check_pattern = "Tea|tea"
  ))

  if (type == "eem") {
    # check if the wavelengths are different across data if so, stop and provide info
    ex <- unique(get_sample_info(blank, "ex"))
    em <- unique(get_sample_info(blank, "em"))

    if (nrow(ex) > 1 | nrow(em) > 1) {
      stop(paste0(
        "Excitation and/or emission wavelengths are inconsistent across analytical blanks.",
        "\nUse staRdom::eem_checksize to identify samples with larger ranges and staRdom::eem_extend2largest to fill in missing wavlengths."
      ))
    }

    # blank correct blanks
    blank_eems <- add_blanks(blank, validate = FALSE)

    # blank subtract
    blank_eems <- subtract_blank(blank_eems)

    # raman normalize
    blank_eems <- raman_normalize(blank_eems)

    # make into a giant df
    blank_df <- lapply(blank_eems, eem_transform) %>%
      dplyr::bind_rows() %>%
      dplyr::group_by(.data$ex, .data$em) %>%
      dplyr::summarise(mean = mean(.data$fluor, na.rm = TRUE), sdev = sd(.data$fluor, na.rm = TRUE)) %>%
      mutate(mdl = (.data$sdev * 3 + .data$mean)) %>%
      dplyr::select("ex", "em", "mdl")

    # turn into a eem object
    dates <- get_sample_info(blank_eems, "analysis_date")
    mdl_eem <- list(
      file = file.path(qaqc_dir, "eem-mdl.rds"),
      sample = "long-term-mdl",
      x = matrix(data = blank_df$mdl, nrow = length(unique(blank_df$em)), ncol = length(unique(blank_df$ex))),
      ex = unique(blank_df$ex),
      em = unique(blank_df$em),
      location = dir,
      meta_name = "long-term-mdl",
      dilution = NA,
      integration_time_s = NA,
      raman_area_1s = NA,
      analysis_date = paste(min(dates, na.rm = TRUE), max(dates, na.rm = TRUE), sep = ":"),
      description = "long-term method detection limit for EEMs",
      doc_mgL = NA,
      notes = paste0(
        "long-term method detection limit (MDL) for EEMs samples based on ", length(blank_eems), " samples collected from ",
        min(dates, na.rm = TRUE), " to ", max(dates, na.rm = TRUE),
        ". MDL calcuated as the standard devation x 3 + mean of the long term blank-corrected and raman-normalized, analytical blanks.",
        "the location description is the directory where the long-term blanks came from."
      )
    )

    # ensure correct attributes
    mostattributes(mdl_eem) <- attributes(blank_eems[[1]])
    names(mdl_eem) <- names(blank_eems[[1]])[-c(15:16)]

    # cache mdl data
    if (qaqc_dir != FALSE) {
      saveRDS(mdl_eem, file.path(qaqc_dir, "eem-mdl.rds"))
    } else {
      return(mdl_eem)
    }
  }

  if (type == "abs") {
    # check if the wavelengths are different across data if so, stop and provide info
    data <- get_sample_info(blank, "data")
    missing <- sum(sapply(data, is.na))

    if (missing > 0) {
      stop(paste0(
        "Absorbance wavelengths are inconsistent across analytical blanks.",
        "\nPlease use eemanalyzeR::abs_interp to interpolate across the missing wavlengths."
      ))
    }

    # make a giant df
    blank_abs_df <- get_sample_info(blank, "data") %>%
      as.data.frame() %>%
      pivot_longer(-"wavelength", names_to = "sample", values_to = "abs")

    # warn if there's suspicious samples to double check
    check <- blank_abs_df %>%
      dplyr::group_by(.data$sample) %>%
      dplyr::summarise(
        max = max(abs, na.rm = TRUE),
        min = min(abs, na.rm = TRUE)
      )
    high <- check %>% dplyr::filter(.data$max > 0.05)
    if (nrow(high) > 0) {
      warning("the following samples have high absorbance, please check:\n", paste(high$sample, collapse = "\n"))
    }

    neg <- check %>% dplyr::filter(.data$min < -0.01 | .data$max < -0.001)
    if (nrow(neg) > 0) {
      warning("the following samples have substantial negative absorbance, please check:\n", paste(neg$sample, collapse = "\n"))
    }

    # get mean and sd across all wavelengths
    abs_mdls <- blank_abs_df %>%
      select(-"sample") %>%
      dplyr::group_by(.data$wavelength) %>%
      dplyr::summarise(
        mean = mean(abs, na.rm = TRUE),
        sdev = sd(abs, na.rm = TRUE)
      ) %>%
      mutate(mdl = (.data$sdev * 3 + .data$mean)) %>%
      dplyr::select("wavelength", "mdl")

    # turn into a abs object
    dates <- get_sample_info(blank, "analysis_date")
    mdl_abs <- list(
      file = file.path(qaqc_dir, "abs-mdl.rds"),
      sample = "long-term-mdl",
      n = length(unique(blank_abs_df$wavelength)),
      data = unname(as.matrix(abs_mdls[order(abs_mdls$wavelength, decreasing = TRUE), ])),
      location = dir,
      meta_name = "long-term-mdl",
      dilution = NA,
      analysis_date = paste(min(dates, na.rm = TRUE), max(dates, na.rm = TRUE), sep = ":"),
      description = "long-term method detection limit for absorbance",
      doc_mgL = NA,
      notes = paste0(
        "long-term method detection limit (MDL) for absorbance samples based on ", length(blank), " samples collected from ",
        min(dates, na.rm = TRUE), " to ", max(dates, na.rm = TRUE),
        ". MDL calcuated as the standard devation x 3 + mean of the long term blank-corrected and raman-normalized, analytical blanks.",
        "the location description is the directory where the long-term blanks came from."
      )
    )

    # ensure correct attributes
    mostattributes(mdl_abs) <- attributes(blank[[1]])
    names(mdl_abs) <- names(blank[[1]])

    # cache mdl data
    if (qaqc_dir != FALSE) {
      saveRDS(mdl_abs, file.path(qaqc_dir, "abs-mdl.rds"))
    } else {
      return(mdl_abs)
    }
  }
}
