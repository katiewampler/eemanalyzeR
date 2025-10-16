#TODO: where to save

get_mdl <- function(dir, pattern="BLK", type = "eem", output_dir=NULL){
  stopifnot(type %in% c("eem", "abs"), dir.exists(dir))

  if(is.null(output_dir)){output_dir <- rappdirs::user_data_dir()}

  if(type == "eem"){
    #get all blanks in directory with instrument blanks
      blank_eems <- eem_dir_read(file.path(dir, "eem"), pattern=pattern)

    #get metadata for raman area
      blank_meta <- read.csv(file.path(dir, "merged-blk-metadata.csv"))
      blank_meta$data_identifier <- blank_meta$long_term_name

      blank_eems <- add_metadata(blank_meta, blank_eems)

    #blank correct blanks
      blank_eems <- add_blanks(blank_eems, validate = FALSE, pattern="blank")

    #blank subtract
      blank_eems <- subtract_blank(blank_eems)

    #raman normalize
      blank_eems <- raman_normalize(blank_eems)

    #make into a giant df
      eem_to_df <- function(eem){
        df <- data.frame(ex= rep(eem$ex, each=length(eem$em)),
                   em = rep(eem$em, length(eem$ex)),
                   fluor = as.vector(eem$x))

        return(df)
      }
      blank_df <- lapply(blank_eems, eem_to_df) %>% dplyr::bind_rows() %>%
        dplyr::group_by(.data$ex, .data$em) %>%
        dplyr::summarise(mean = mean(fluor, na.rm = TRUE), sdev = sd(fluor, na.rm = TRUE)) %>%
        mutate(mdl = (.data$sdev * 3 + .data$mean)) %>%
        dplyr::select("ex", "em", "mdl")

    #save plot
      ggplot(blank_df %>% dplyr::filter(mdl < 0.4), aes(x=ex, y=em, fill=mdl)) + ggplot2::geom_tile() +
        labs(title = "long-term mdl plot (values above 0.4 filtered out")

    #turn into a eem object
      dates <- get_sample_info(blank_eems, "analysis_date")
      mdl_eem <- list(file= NA,
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
      class(mdl_eem) <- "eem"


      #start of code to cache mdl data
      path <- file.path(rappdirs::user_data_dir(appname = "eemanalyzeR"))
      dir.create(path, showWarnings = FALSE, recursive = TRUE)

  }

  if(type == "abs"){
    #load all the blank absorbance
      blank_abs <- abs_dir_read(file.path(dir, "abs"), pattern=pattern)

    #get metadata
      blank_meta <- read.csv(file.path(dir, "merged-blk-metadata.csv"))
      blank_meta$data_identifier <- blank_meta$long_term_name

      blank_abs <- add_metadata(blank_meta, blank_abs)

    #make a giant df
      blank_abs_df <- blank_abs %>% purrr::map(function(x){as.data.frame(x[["data"]])}) %>% dplyr::bind_rows()
      colnames(blank_abs_df) <- c("wavelength", "abs")

    #get mean and sd across all wavelengths
      abs_mdls <- blank_abs_df %>% dplyr::group_by(.data$wavelength) %>% dplyr::filter(abs < 5) %>%
        dplyr::summarise(mean = mean(abs, na.rm = TRUE),
                         sdev = sd(abs, na.rm = TRUE)) %>% mutate(mdl = (.data$sdev * 3 + .data$mean)) %>%
        dplyr::select("wavelength", "mdl")

    #visualize (save)
      ggplot(abs_mdls, aes(x=.data$wavelength, y=.data$mdl)) + ggplot2::geom_line()

    #turn into a abs object
      dates <- get_sample_info(blank_abs, "analysis_date")
      mdl_abs <- list(file= NA,
                      sample="long-term-mdl",
                      n = length(unique(blank_abs_df$wavelength)),
                      data = unname(as.matrix(abs_mdls[order(abs_mdls$wavelength, decreasing=TRUE),])),
                      location=dir,
                      meta_name="long-term-mdl",
                      dilution=NA,
                      analysis_date = paste(min(dates, na.rm=TRUE), max(dates, na.rm=TRUE), sep=":"),
                      description = "long-term method detection limit for absorbance",
                      doc_mgL = NA,
                      notes=paste0("long-term method detection limit (MDL) for absorbance samples based on ", length(blank_eems), " samples collected from ",
                                   min(dates, na.rm=TRUE), " to ", max(dates, na.rm=TRUE),
                                   ". MDL calcuated as the standard devation x 3 + mean of the long term blank-corrected and raman-normalized, analytical blanks.",
                                   "the location description is the directory where the long-term blanks came from."))
      class(mdl_abs) <- "abs"
  }
}
