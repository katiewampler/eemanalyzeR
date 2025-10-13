## code to prepare `DATASET` dataset goes here
library(pbapply)

#get full size example files ------
downscale_eems <- function(file, factor=6){
  #downscale EEMs
  if(grepl("SEM|BEM", file)){
    data <- readLines(file.path("data-raw/fullsize data",file)) #read in file

    line1 <- unlist(stringr::str_extract_all(data[1], "-?\\d+(?:\\.\\d*)?(?:[eE][+\\-]?\\d+)?")) #removes tabs and wavelength
    line1 <- line1[seq(1, length(line1), 6)]
    line1 <- paste(c("Wavelength", line1), collapse="\t")

    #repeat for all the other wavelengths
    keep <- seq(1, 250, by=6)+ 1

    results <- data[keep]
    for(x in 1:length(results)){
      line <- unlist(stringr::str_extract_all(results[x], "-?\\d+(?:\\.\\d*)?(?:[eE][+\\-]?\\d+)?")) #removes tabs and wavelength
      line <- line[c(1,seq(2, length(line), 6))]
      line <- paste(line, collapse="\t")
      if(x == 1){
        line2 <- line
      }else(line2 <- c(line2, line))
    }

    data <- c(line1, line2)
    readr::write_lines(data, file.path("data-raw",file))
  }

  #downscale absorbance
  if(grepl("ABS", file)){
    data <- readLines(file.path("data-raw/fullsize data",file)) #read in file
    data <- data[seq(1, length(data), 6)]
    readr::write_lines(data, file.path("data-raw",file))

  }}

files <- list.files("data-raw/fullsize data", pattern=".dat")

#downscale and save
for(x in files){
  downscale_eems(x)
}

#read into R environment
  #read in samples and blanks
  example_eems <- eem_dir_read("data-raw", pattern="SEM|BEM")

  #read in absorbance
  example_absorbance <- abs_dir_read("data-raw", pattern="ABS[.]dat")

    usethis::use_data(example_eems, overwrite = T)
    usethis::use_data(example_absorbance, overwrite = T)

  metadata <- meta_read("data-raw")
  usethis::use_data(metadata, overwrite = T)


#code to create long term tea and blanks -----
  #pull files from Aqualog folder and put in raw data
    dir <- "T:/Research/Aqualog_Data/2_PNNL_DOM"
    dates <- list.files(dir, "[0-9]{4}_[0-9]{2}_[0-9]{2}")
    output_dir <- "data-raw/long-term-standards/blanks"
    get_blank_info <- function(file, output_dir){
      #get info about samples and metadata
        blk_files <- list.files(file.path(dir, file),"blk.*\\.dat|BLK.*\\.dat|Blk.*\\.dat", recursive = TRUE)

        meta_name <- list.files(file.path(dir, file), "metadata.rds", recursive = TRUE, full.names = TRUE)
        if(length(meta_name) > 0){
          meta_file <- readRDS(meta_name)

          #get metadata just for blanks
          meta_file <- meta_file[sapply(meta_file$data_identifier, function(x){any(grepl(x, blk_files))}),]

          if(nrow(meta_file) > 0){
            #make unique name based on folder name (if run morn/even blanks are replicated)
            meta_file$long_term_name <- paste(file, meta_file$data_identifier, sep="_")

            blk_names <- blk_files
            for(x in 1:nrow(meta_file)){
              blk_names <- gsub(meta_file$data_identifier[x], meta_file$long_term_name[x], blk_names)
            }

            blk_name <- basename(blk_names)

            #copy files to new location
            abs <- grepl("1_Absorbance", blk_files)
            eem <- grepl("2_Blanks|3_Samples", blk_files)

            file.copy(file.path(dir, file, blk_files[abs]), paste(output_dir, "abs", blk_name[abs], sep="/"), overwrite = TRUE)
            file.copy(file.path(dir, file, blk_files[eem]), paste(output_dir, "eem", blk_name[eem], sep="/"), overwrite = TRUE)

            #write metadata
            meta_file <- meta_file %>%
              dplyr::select(analysis_date, data_identifier, replicate_no, integration_time_s, dilution, RSU_area_1s, long_term_name)

            write.csv(meta_file, file.path(output_dir, paste("metadata/", file, "_metadata.csv")), row.names = FALSE, quote = FALSE)

          }}
    }

    pblapply(dates, get_blank_info, output_dir=output_dir)

  #combine metadata and save
    meta <- list.files(file.path(output_dir, "metadata"))
    metadata <- lapply(meta, function(x){read.csv(file.path(output_dir, "metadata", x))}) %>% dplyr::bind_rows()
    write.csv(metadata, file.path(output_dir, "merged-blk-metadata.csv"), row.names=FALSE)

#do the same for the tea samples
    #pull files from Aqualog folder and put in raw data
    dir <- "T:/Research/Aqualog_Data/2_PNNL_DOM"
    dates <- list.files(dir, "[0-9]{4}_[0-9]{2}_[0-9]{2}")
    output_dir <- "data-raw/long-term-standards/tea-standards"
    get_tea_info <- function(file, output_dir){
      #get info about samples and metadata
      tea_files <- list.files(file.path(dir, file),"postTea.*\\.dat|preTea.*\\.dat", recursive = TRUE)

      meta_name <- list.files(file.path(dir, file), "metadata.rds", recursive = TRUE, full.names = TRUE)
      if(length(meta_name) > 0){
        meta_file <- readRDS(meta_name)

        #get metadata just for blanks
        meta_file <- meta_file[sapply(meta_file$data_identifier, function(x){any(grepl(x, tea_files))}),]

        if(nrow(meta_file) > 0){
          #make unique name based on folder name (if run morn/even blanks are replicated)
          meta_file$long_term_name <- paste(file, meta_file$data_identifier, sep="_")

          tea_names <- tea_files
          for(x in 1:nrow(meta_file)){
            tea_names <- gsub(meta_file$data_identifier[x], meta_file$long_term_name[x], tea_names)
          }

          tea_name <- basename(tea_names)

          #copy files to new location
          abs <- grepl("1_Absorbance", tea_files)
          eem <- grepl("2_Blanks|3_Samples", tea_files)

          file.copy(file.path(dir, file, tea_files[abs]), paste(output_dir, "abs", tea_name[abs], sep="/"), overwrite = TRUE)
          file.copy(file.path(dir, file, tea_files[eem]), paste(output_dir, "eem", tea_name[eem], sep="/"), overwrite = TRUE)

          #write metadata
          meta_file <- meta_file %>%
            dplyr::select(analysis_date, data_identifier, replicate_no, integration_time_s, dilution, RSU_area_1s, long_term_name)

          write.csv(meta_file, file.path(output_dir, paste0("metadata/", file, "_metadata.csv")), row.names = FALSE, quote = FALSE)

        }}
    }

    pblapply(dates, get_tea_info, output_dir=output_dir)

    #combine metadata and save
    meta <- list.files(file.path(output_dir, "metadata"))
    metadata <- lapply(meta, function(x){read.csv(file.path(output_dir, "metadata", x))}) %>% dplyr::bind_rows()
    write.csv(metadata, file.path(output_dir, "merged-tea-metadata.csv"), row.names=FALSE)

  usethis::use_data(longterm_blank, overwrite = T)

#save index ranges as data.frame ------
  make_na <- function(df){
    df_fix <- df %>%
      dplyr::mutate(dplyr::across(where(is.character), ~ dplyr::na_if(.x, "N/A"))) %>%
      dplyr::mutate(dplyr::across(where(is.numeric), ~ dplyr::na_if(.x, -9999))) %>%
      dplyr::mutate(dplyr::across(where(lubridate::is.POSIXct), ~ dplyr::na_if(.x, as.Date("9999-12-31 00:00:00")))) %>%
      dplyr::mutate(dplyr::across(where(lubridate::is.Date), ~ dplyr::na_if(.x, as.Date("9999-12-31"))))

    return(df_fix)
  }

  indice_ranges <- read.csv("data-raw/indice-ranges.csv")

  #make long for easier use
  indice_ranges <- indice_ranges %>% make_na() %>% pivot_longer(eemanalyzeR:eemR, names_to = "index_method", values_to="index") %>%
    dplyr::select(index_method, index, low_val, high_val, sources)
  indice_ranges <- indice_ranges[!is.na(indice_ranges$index),]

  usethis::use_data(indice_ranges, overwrite = T)
