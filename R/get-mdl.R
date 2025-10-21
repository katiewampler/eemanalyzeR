#' Calculate method detection limits
#'
#' The method detection limit (MDL) is the signal limit that we can reliably have confidence
#' that the result is greater than zero and distinguishable from blanks. In this case the MDL
#' is calculated based on the method proposed by Hansen et al. (2018); three times the standard
#' deviation plus the mean of the long-term blank.
#'
#' @param dir path to folder containing long term EEMs and/or absorbance files
#' @param meta_name name of metadata file. optional if metadata is only xlsx or csv file in input_dir
#' if not specified function will attempt to load any xlsx or csv file in directory and return an error if there is more than one
#' @param sheet name of sheet containing metadata. only required if metadata isn't the first sheet
#' @param pattern optional. a character string containing a \code{\link[base]{regular expression}}
#' to be matched to the files in input_dir.
#' only files matching the pattern will be loaded.
#' @param type which MDL to calculate: either `eem` or `abs`
#' @param recursive logical. should the function recurse into directories?
#' @param output_dir the location to save the mdl file to, default is a user-specific data directory (\link[rappdirs]{user_data_dir}). If
#' FALSE will return the MDL instead of saving.
#'
#' @returns
#' - If output_dir is FALSE, returns an `eem` or `abs` object containing the MDL values.
#' - Otherwise it saves a .RDS file containing the MDL data formatted as either an `eem` or `abs` object to the specified output_dir.
#' @md
#' @export
#'
#' @details
#' To calculate the MDL you need:
#'
#' - A directory containing analytical blanks with their associated instrument blanks (less than 20 will prompt a warning)
#'    - Note: sample names must be unique
#'
#' - Metadata for the blanks including (at a minimum) the integration time and raman area in a single file,
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
#' eem_mdl <- get_mdl(file.path(system.file("extdata", package = "eemanalyzeR"), "long-term-blanks"),
#' meta_name="longtermblank-metadata.csv", pattern = "longtermblank",
#' type="eem", output_dir = FALSE)
#'
#' plot_eem(eem_mdl)
#'
get_mdl <- function(dir, meta_name=NULL, sheet=NULL, pattern="BLK", type = "eem", recursive=FALSE, output_dir=NULL){
  stopifnot(type %in% c("eem", "abs"), dir.exists(dir))

  #set up file structure for saving mdl data
    if(is.null(output_dir)){output_dir <- file.path(rappdirs::user_data_dir(appname = "eemanalyzeR"), "qaqc-stds")}
    if(output_dir != FALSE){dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)}

  #get metadata
    blank_meta <- meta_read(dir, name=meta_name, sheet=sheet, validate = FALSE)

  #get all blanks in directory with instrument blanks
    if(type == "eem"){blank <- eem_dir_read(dir, pattern=pattern, recursive=recursive)}
    if(type == "abs"){blank <- abs_dir_read(dir, pattern=pattern, recursive=recursive)}

  #check number of samples
    n_samps <- length(blank)/2
    if(n_samps < 20){warning("Calculating MDL based on less than 20 samples, MDL may be unreliable")}

  #add metadata
    blank <- add_metadata(blank_meta, blank)

  if(type == "eem"){
    #check if the wavelengths are different across data if so, stop and provide info
    ex <- unique(get_sample_info(blank, "ex"))
    em <- unique(get_sample_info(blank, "em"))

    if(nrow(ex) > 1 | nrow(em) > 1){
      stop(paste0("Excitation and/or emission wavelengths are inconsistent across analytical blanks.",
                  "\nUse staRdom::eem_checksize to identify samples with larger ranges and staRdom::eem_extend2largest to fill in missing wavlengths."))
    }

    #blank correct blanks
      blank_eems <- add_blanks(blank, validate = FALSE, pattern="BEM")

    #blank subtract
      blank_eems <- subtract_blank(blank_eems)

    #raman normalize
      blank_eems <- raman_normalize(blank_eems)

    #make into a giant df
      blank_df <- lapply(blank_eems, eem_flatten) %>% dplyr::bind_rows() %>%
        dplyr::group_by(.data$ex, .data$em) %>%
        dplyr::summarise(mean = mean(.data$fluor, na.rm = TRUE), sdev = sd(.data$fluor, na.rm = TRUE)) %>%
        mutate(mdl = (.data$sdev * 3 + .data$mean)) %>%
        dplyr::select("ex", "em", "mdl")

    #turn into a eem object
      dates <- get_sample_info(blank_eems, "analysis_date")
      mdl_eem <- list(file= file.path(output_dir, "eem-mdl.Rds"),
                      sample="long-term-mdl",
                      x = matrix(data=blank_df$mdl, nrow=length(unique(blank_df$em)), ncol=length(unique(blank_df$ex))),
                      ex = unique(blank_df$ex),
                      em = unique(blank_df$em),
                      location=dir,
                      meta_name="long-term-mdl",
                      dilution=NA,
                      integration_time_s = NA,
                      raman_area_1s = NA,
                      analysis_date = paste(min(dates, na.rm=TRUE), max(dates, na.rm=TRUE), sep=":"),
                      description = "long-term method detection limit for EEMs",
                      doc_mgL = NA,
                      notes=paste0("long-term method detection limit (MDL) for EEMs samples based on ", length(blank_eems), " samples collected from ",
                                        min(dates, na.rm=TRUE), " to ", max(dates, na.rm=TRUE),
                                        ". MDL calcuated as the standard devation x 3 + mean of the long term blank-corrected and raman-normalized, analytical blanks.",
                                        "the location description is the directory where the long-term blanks came from."))

      #ensure correct attributes
        mostattributes(mdl_eem) <- attributes(blank_eems[[1]])
        names(mdl_eem) <- names(blank_eems[[1]])[-c(15:16)]

      #cache mdl data
      if(output_dir != FALSE){
        saveRDS(mdl_eem, file.path(output_dir, "eem-mdl.Rds"))
      }else{
        return(mdl_eem)
      }


  }

  if(type == "abs"){
    #check if the wavelengths are different across data if so, stop and provide info
      data <- get_sample_info(blank, "data")
      missing <- sum(sapply(data, is.na))

      if(missing > 0){
        stop(paste0("Absorbance wavelengths are inconsistent across analytical blanks.",
                    "\nPlease interpolate across the missing wavlengths."))
      }

    #make a giant df
      blank_abs_df <- get_sample_info(blank, "data") %>% as.data.frame() %>%
        pivot_longer(-"wavelength", names_to = "sample", values_to = "abs") %>%
        dplyr::select(-"sample")

    #get mean and sd across all wavelengths
      abs_mdls <- blank_abs_df %>% dplyr::group_by(.data$wavelength) %>% dplyr::filter(abs < 5) %>%
        dplyr::summarise(mean = mean(abs, na.rm = TRUE),
                         sdev = sd(abs, na.rm = TRUE)) %>% mutate(mdl = (.data$sdev * 3 + .data$mean)) %>%
        dplyr::select("wavelength", "mdl")

    #turn into a abs object
      dates <- get_sample_info(blank, "analysis_date")
      mdl_abs <- list(file= file.path(output_dir, "abs-mdl.Rds"),
                      sample="long-term-mdl",
                      n = length(unique(blank_abs_df$wavelength)),
                      data = unname(as.matrix(abs_mdls[order(abs_mdls$wavelength, decreasing=TRUE),])),
                      location=dir,
                      meta_name="long-term-mdl",
                      dilution=NA,
                      analysis_date = paste(min(dates, na.rm=TRUE), max(dates, na.rm=TRUE), sep=":"),
                      description = "long-term method detection limit for absorbance",
                      doc_mgL = NA,
                      notes=paste0("long-term method detection limit (MDL) for absorbance samples based on ", length(blank), " samples collected from ",
                                   min(dates, na.rm=TRUE), " to ", max(dates, na.rm=TRUE),
                                   ". MDL calcuated as the standard devation x 3 + mean of the long term blank-corrected and raman-normalized, analytical blanks.",
                                   "the location description is the directory where the long-term blanks came from."))

    #ensure correct attributes
      mostattributes(mdl_abs) <- attributes(blank[[1]])
      names(mdl_abs) <- names(blank[[1]])

      #cache mdl data
      if(output_dir != FALSE){
        saveRDS(mdl_abs, file.path(output_dir, "abs-mdl.Rds"))
      }else{
        return(mdl_abs)
      }

  }
}
