#' Create long term average check standard
#'
#' Calculates the average absorbance and fluorescence values given multiple check
#' standards to create a long-term standard that the daily check standard can be
#' checked against for consistency.
#'
#' @param dir Path to a folder containing long-term EEMs and/or absorbance files.
#'   All files in this directory will be loaded.
#' @param meta_name Name of the metadata file. Optional if the metadata file is the
#'   only `.xlsx` or `.csv` file in `dir`. If not specified, the function attempts to find
#'   a single metadata file and errors if multiple files are present.
#' @param sheet Name of the sheet containing metadata (only required
#'   if the metadata is not on the first sheet of an `.xlsx` file).
#' @param abs_pattern A character string containing a
#'   [base::regular expression] used to identify absorbance files.
#' @param iblank Optional. A character string containing a
#'   [base::regular expression] used to identify instrument blanks.
#' @param type Which MDL to calculate: either "eem" or "abs".
#' @param recursive Logical. Should the function recursively search directories?
#' @param qaqc_dir Directory in which to save the QAQC `.rds` file.
#'   Default: a user-specific data directory via [rappdirs::user_data_dir()].
#'   If `FALSE`, the function returns the MDL object instead of saving it.
#'
#' @returns
#' - If `dir = FALSE`: an `eem` or `abs` object containing the averaged check standard values.
#' - Otherwise: saves an `.rds` file containing the averaged check standard and
#'   invisibly returns the file path.
#'
#' @md
#' @export
#'
#' @details
#' To ensure optical measurements are consistent across days, Hansen et al. (2018)
#' recommend using a standard reference material: Pure Leaf® unsweetened black tea.
#' The tea standard should be diluted to 1% concentration before analysis. It is
#' recommended to run the tea standard at the start and end of each analysis run.
#' According to USGS standards, tea standard indices measured on a given day should
#' fall within 20% of the long-term average (Hansen et al. 2018).
#'
#' To calculate the average check standard you need:
#'
#' - A directory containing check standards with their associated instrument blanks (less than 20 will prompt a warning)
#'    - Note: sample names must be unique
#'
#' - Metadata for the check samples including (at a minimum) the integration time and raman area in a single file,
#' formatted as a metadata file (see [eemanalyzeR::metadata])
#'
#'
#' @source
#' Hansen, A. M., Fleck, J., Kraus, T. E. C., Downing, B. D., von Dessonneck, T., & Bergamaschi, B. (2018).
#' *Procedures for using the Horiba Scientific Aqualog® fluorometer to measure absorbance
#' and fluorescence from dissolved organic matter* (USGS Numbered Series No. 2018-1096).
#' U.S. Geological Survey.
#' <https://doi.org/10.3133/ofr20181096>
#'
#' @examples
#' eem_std <- create_std(file.path(system.file("extdata", package = "eemanalyzeR"),"long-term-std"),
#' meta_name="longterm-checkstd-metadata.csv", abs_pattern = "ABS",
#' type="eem", qaqc_dir = FALSE)
#'
#' plot(eem_std)
#'

create_std <- function(dir, meta_name=NULL, sheet=NULL, abs_pattern="Abs", iblank="BEM",
                        type = "eem", recursive=FALSE, qaqc_dir=NULL){
  stopifnot(type %in% c("eem", "abs"), dir.exists(dir))

  #set up file structure for saving mdl data
    if(is.null(qaqc_dir)){qaqc_dir <- file.path(rappdirs::user_data_dir(appname = "eemanalyzeR"), "qaqc-stds")}
    if(qaqc_dir != FALSE){dir.create(qaqc_dir, showWarnings = FALSE, recursive = TRUE)}

  #get metadata
    if(!is.null(meta_name)){input <- file.path(dir, meta_name)}else{input <- dir}
    tea_meta <- meta_read(input, sheet=sheet)

  #get all tea samples in directory with instrument blanks
    tea_abs <- abs_dir_read(dir, recursive=recursive, pattern=abs_pattern)

    if(type == "eem"){tea <- eem_dir_read(dir, recursive=recursive)}
    if(type == "abs"){tea <- tea_abs}

  #check number of samples
    n_samps <- length(tea)

    #error if samples are missing, likely a pattern issue
    if(length(tea_abs) == 0){stop("No absorbance was found to load. Please check your abs_pattern arugment.")}
    if(length(tea) == 0){stop("No fluoresence data was found to load. Please check your eem_pattern arugment.")}

    if(type == "eem"){n_samps <- n_samps/2}
    if(n_samps < 20){warning("Calculating average check standard based on less than 20 samples, average may be unreliable")}

  #add metadata
    tea <- add_metadata(tea_meta, tea)

  if(type == "eem"){
    #check if the wavelengths are different across data if so, stop and provide info
    ex <- unique(get_sample_info(tea, "ex"), MARGIN = 2)
    em <- unique(get_sample_info(tea, "em"), MARGIN = 2)

    if(ncol(ex) > 1 | ncol(em) > 1){
      stop(paste0("Excitation and/or emission wavelengths are inconsistent across tea standards.",
      "\nUse staRdom::eem_checksize to identify samples with larger ranges and staRdom::eem_extend2largest to fill in missing wavlengths."))
    }

    #get absorbance data to ife correct
    tea_abs <- add_metadata(tea_meta, tea_abs)

    #blank subtract
    tea_eems <- add_blanks(tea, validate = FALSE)
    tea_eems <- subtract_blank(tea_eems)

    #remove scattering
    tea_eems <- remove_scattering(tea_eems)

    #ife correct
    tea_eems <- ife_correct(tea_eems, tea_abs)

    #raman normalize
    tea_eems <- raman_normalize(tea_eems)

    #correct dilution
    tea_eems <- correct_dilution(tea_eems)

    #get all the data matrices
    data <- lapply(tea_eems, function(eem){eem$x})

    #average across all wavelengths
    mean <- Reduce("+", data) / length(data)

    #turn into a eem object
    dates <- get_sample_info(tea_eems, "analysis_date")
    tea_eem <- list(file= file.path(qaqc_dir, "eem-check-std.rds"),
                    sample="long-term-check-std",
                    x = mean,
                    ex = get_sample_info(tea_eems, "ex")[,1],
                    em = get_sample_info(tea_eems, "em")[,1],
                    location=dir,
                    meta_name="long-term-check-std",
                    dilution=NA,
                    integration_time_s = NA,
                    raman_area_1s = NA,
                    analysis_date = paste(min(dates, na.rm=TRUE), max(dates, na.rm=TRUE), sep=":"),
                    description = "long-term average of check standard for EEMs",
                    doc_mgL = NA,
                    notes=paste0("long-term average of check standard for EEMs based on ", length(tea_eems), " samples collected from ",
                                 min(dates, na.rm=TRUE), " to ", max(dates, na.rm=TRUE),
                                 ". Check standards have been blank-corrected and raman-normalized.",
                                 "the location description is the directory where the long-term blanks came from."))

    #ensure correct attributes
    mostattributes(tea_eem) <- attributes(tea_eems[[1]])
    names(tea_eem) <- names(tea_eems[[1]])[-c(15:16)]

    #cache tea data
    if(qaqc_dir != FALSE){
      saveRDS(tea_eem, file.path(qaqc_dir, "eem-check-std.rds"))
    }else{
      return(tea_eem)
    }


  }

  if(type == "abs"){
    #check if the wavelengths are different across data if so, stop and provide info
      data <- get_sample_info(tea, "data")
      missing <- sum(sapply(data, is.na))

      if(missing > 0){
        stop(paste0("Absorbance wavelengths are inconsistent across check standards.",
                    "\nPlease interpolate across the missing wavlengths."))
      }

    #correct for dilution
      #correct dilution
      tea <- correct_dilution(tea)

    #make a giant df
      tea_abs_df <- get_sample_info(tea, "data") %>% as.data.frame() %>%
        pivot_longer(-"wavelength", names_to = "sample", values_to = "abs") %>%
        dplyr::select(-"sample")

    #get mean and sd across all wavelengths
    abs_tea <- tea_abs_df %>% dplyr::group_by(.data$wavelength) %>%
      dplyr::summarise(mean = mean(abs, na.rm = TRUE)) %>%
      dplyr::select("wavelength", "mean")

    #turn into a abs object
    dates <- get_sample_info(tea, "analysis_date")
    tea_abs <- list(file= file.path(qaqc_dir, "abs-check-std.rds"),
                    sample="long-term-check-std",
                    n = length(unique(tea_abs_df$wavelength)),
                    data = unname(as.matrix(abs_tea[order(abs_tea$wavelength, decreasing=TRUE),])),
                    location=dir,
                    meta_name="long-term-check-std",
                    dilution=NA,
                    analysis_date = paste(min(dates, na.rm=TRUE), max(dates, na.rm=TRUE), sep=":"),
                    description = "long-term check standard for absorbance",
                    doc_mgL = NA,
                    notes=paste0("long-term average of check standard absorbance based on ", length(tea), " samples collected from ",
                                 min(dates, na.rm=TRUE), " to ", max(dates, na.rm=TRUE),
                                 ".",
                                 "the location description is the directory where the long-term blanks came from."))

    #ensure correct attributes
    mostattributes(tea_abs) <- attributes(tea[[1]])
    names(tea_abs) <- names(tea[[1]])

    #cache tea data
    if(qaqc_dir != FALSE){
      saveRDS(tea_abs, file.path(qaqc_dir, "abs-check-std.rds"))
    }else{
      return(tea_abs)
    }

  }
}
