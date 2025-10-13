#TODO: where to save

get_MDL <- function(dir, pattern="BLK", type = "eem", output_dir=NULL){
  stopifnot(type %in% c("eem", "abs"), dir.exists(dir))

  if(is.null(output_dir)){output_dir <- tempdir()}

  if(type == "eem"){
    #get all blanks in directory with instrument blanks
      blank_eems <- eem_dir_read(file.path(dir, "eem"), pattern=pattern)

    #get metadata for raman area
      blank_meta <- read.csv(file.path(dir, "merged-blk-metadata.csv"))
      blank_meta$data_identifier <- blank_meta$long_term_name

      test <- add_metadata(blank_meta, blank_eems)

    #blank correct blanks
      add_blanks(blank_meta)

    #blank subtract

    #raman normalize

    #get mdl from data

    #save plot

    #write some info about how generated

  }

  if(type == "abs"){
    #load all the blank absorbance
      blank_abs <- abs_dir_read(dir, pattern=pattern)

    #make a giant df
      blank_abs_df <- blank_abs %>% purrr::map(function(x){as.data.frame(x[["data"]])}) %>% dplyr::bind_rows()
      colnames(blank_abs_df) <- c("wavelength", "abs")

    #get mean and sd across all wavelengths
      abs_mdls <- blank_abs_df %>% dplyr::group_by(wavelength) %>% dplyr::filter(abs < 5) %>%
        dplyr::summarise(mean = mean(abs, na.rm = TRUE),
                         sdev = sd(abs, na.rm = TRUE)) %>% mutate(mdl = (sdev * 3 + mean)) %>%
        dplyr::select(wavelength, mdl)

    #visualize (save)
      ggplot(abs_mdls, aes(x=wavelength, y=mdl)) + ggplot2::geom_line()


    #make some metadata about how generated
  }
}
