#' Create Long Term Average Tea Standard
#'
#' To ensure optical measurements are consistent across days, Hansen et al. (2018) recommend using a standard reference material:
#' Pure Leaf®, unsweetened black tea. The tea standard should be diluted to 1 % concentration before analysis. It is recommended to
#' run the tea standard at the start and end of an analysis run. According to USGS standards, the indices for the tea standards run on
#' a given day should be within 20 % of the long-term average (Hansen et al. 2018).
#'
#' @param dir path to folder containing long term EEMs and/or absorbance files
#' @param meta_name name of metadata file. optional if metadata is only xlsx or csv file in input_dir
#' if not specified function will attempt to load any xlsx or csv file in directory and return an error if there is more than one
#' @param sheet name of sheet containing metadata. only required if metadata isn't the first sheet
#' @param abs_pattern a character string containing a \code{\link[base]{regular expression}}
#' used to specify the sample names of the absorbance data.
#' @param iblank optional. a character string containing a \code{\link[base]{regular expression}}
#' used to specify the sample names of the instrument blanks.
#' @param type which MDL to calculate: either `eem` or `abs`
#' @param recursive logical. should the function recurse into directories?
#' @param output_dir the location to save the mdl file to, default is a user-specific data directory (\link[rappdirs]{user_data_dir}). If
#' FALSE will return the averaged tea standard instead of saving.
#'
#' @returns
#' - If output_dir is FALSE, returns an `eem` or `abs` object containing the average tea values.
#' - Otherwise it saves a .RDS file containing the average tea data formatted as either an `eem` or `abs` object to the specified output_dir.
#' @md
#' @export
#'
#' @details
#' To calculate the average tea standard you need:
#'
#' - A directory containing tea standards with their associated instrument blanks (less than 20 will prompt a warning)
#'    - Note: sample names must be unique
#'
#' - Metadata for the tea samples including (at a minimum) the integration time and raman area in a single file,
#' formatted as a metadata file (see \link[eemanalyzeR]{metadata})
#'
#'
#' @source
#' Hansen, A. M., Fleck, J., Kraus, T. E. C., Downing, B. D., von Dessonneck, T., & Bergamaschi, B. (2018).
#' Procedures for using the Horiba Scientific Aqualog® fluorometer to measure absorbance and fluorescence from dissolved organic matter
#' (USGS Numbered Series No. 2018-1096). Procedures for using the Horiba Scientific Aqualog® fluorometer to measure absorbance and fluorescence
#' from dissolved organic matter (Vol. 2018-1096). Reston, VA: U.S. Geological Survey. \url{https://doi.org/10.3133/ofr20181096}
#'
#' @examples
#' eem_teastd <- create_tea_std(
#' file.path(system.file("extdata", package = "eemanalyzeR"), "long-term-tea"),
#' abs_pattern="ABS",type="eem", output_dir = FALSE)
#'
#' plot(eem_teastd)
#'

create_tea_std <- function(dir, meta_name=NULL, sheet=NULL, abs_pattern="Abs", iblank="BEM",
                        type = "eem", recursive=FALSE, output_dir=NULL){
  stopifnot(type %in% c("eem", "abs"), dir.exists(dir))

  #set up file structure for saving mdl data
    if(is.null(output_dir)){output_dir <- file.path(rappdirs::user_data_dir(appname = "eemanalyzeR"), "qaqc-stds")}
    if(output_dir != FALSE){dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)}

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
    if(n_samps < 20){warning("Calculating average tea standard based on less than 20 samples, average may be unreliable")}

  #add metadata
    tea <- add_metadata(tea_meta, tea)

  if(type == "eem"){
    #check if the wavelengths are different across data if so, stop and provide info
    ex <- unique(get_sample_info(tea, "ex"))
    em <- unique(get_sample_info(tea, "em"))

    if(nrow(ex) > 1 | nrow(em) > 1){
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
    tea_eem <- list(file= file.path(output_dir, "eem-tea-std.Rds"),
                    sample="long-term-tea-std",
                    x = mean,
                    ex = get_sample_info(tea_eems, "ex")[1,],
                    em = get_sample_info(tea_eems, "em")[1,],
                    location=dir,
                    meta_name="long-term-tea-std",
                    dilution=NA,
                    integration_time_s = NA,
                    raman_area_1s = NA,
                    analysis_date = paste(min(dates, na.rm=TRUE), max(dates, na.rm=TRUE), sep=":"),
                    description = "long-term average of tea standard for EEMs",
                    doc_mgL = NA,
                    notes=paste0("long-term average of tea standard for EEMs based on ", length(tea_eems), " samples collected from ",
                                 min(dates, na.rm=TRUE), " to ", max(dates, na.rm=TRUE),
                                 ". Tea standards have been blank-corrected and raman-normalized.",
                                 "the location description is the directory where the long-term blanks came from."))

    #ensure correct attributes
    mostattributes(tea_eem) <- attributes(tea_eems[[1]])
    names(tea_eem) <- names(tea_eems[[1]])[-c(15:16)]

    #cache tea data
    if(output_dir != FALSE){
      saveRDS(tea_eem, file.path(output_dir, "eem-tea-std.Rds"))
    }else{
      return(tea_eem)
    }


  }

  if(type == "abs"){
    #check if the wavelengths are different across data if so, stop and provide info
      data <- get_sample_info(tea, "data")
      missing <- sum(sapply(data, is.na))

      if(missing > 0){
        stop(paste0("Absorbance wavelengths are inconsistent across tea standards.",
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
    abs_tea <- tea_abs_df %>% dplyr::group_by(.data$wavelength) %>% dplyr::filter(abs < 5) %>%
      dplyr::summarise(mean = mean(abs, na.rm = TRUE)) %>%
      dplyr::select("wavelength", "mean")

    #turn into a abs object
    dates <- get_sample_info(tea, "analysis_date")
    tea_abs <- list(file= file.path(output_dir, "abs-tea-std.Rds"),
                    sample="long-term-tea-std",
                    n = length(unique(tea_abs_df$wavelength)),
                    data = unname(as.matrix(abs_tea[order(abs_tea$wavelength, decreasing=TRUE),])),
                    location=dir,
                    meta_name="long-term-tea-std",
                    dilution=NA,
                    analysis_date = paste(min(dates, na.rm=TRUE), max(dates, na.rm=TRUE), sep=":"),
                    description = "long-term tea standard for absorbance",
                    doc_mgL = NA,
                    notes=paste0("long-term average of tea standard absorbance based on ", length(tea), " samples collected from ",
                                 min(dates, na.rm=TRUE), " to ", max(dates, na.rm=TRUE),
                                 ".",
                                 "the location description is the directory where the long-term blanks came from."))

    #ensure correct attributes
    mostattributes(tea_abs) <- attributes(tea[[1]])
    names(tea_abs) <- names(tea[[1]])

    #cache tea data
    if(output_dir != FALSE){
      saveRDS(tea_abs, file.path(output_dir, "abs-tea-std.Rds"))
    }else{
      return(tea_abs)
    }

  }
}
