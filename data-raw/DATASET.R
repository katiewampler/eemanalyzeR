## code to prepare `DATASET` dataset goes here
library(pbapply)

#get full size example files ------
downscale_eems <- function(file, factor=6){
  #downscale EEMs
  if(grepl("SEM|BEM|eem|Waterfall", file)){
    data <- readLines(file) #read in file

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
    readr::write_lines(data, file.path("data-raw",basename(file)))
  }

  #downscale absorbance
  if(grepl("ABS|abs|Abs", file)){
    data <- readLines(file) #read in file

    #check for manual format, if so deal with differently
    if(grepl("nm\tuA", data[2])){
      end_abs <- grep("Wavelength\t", data)[2]
      data <- c(data[1:3],data[seq(4, end_abs, 6)])
    }else{
      data <- data[seq(1, length(data), 6)]
    }
    readr::write_lines(data, file.path("data-raw",basename(file)))

  }}

files <- list.files("data-raw/fullsize-data", pattern=".dat", full.names = TRUE)

#downscale and save
for(x in files){
  downscale_eems(x)
}

#read into R environment
  #read in samples and blanks
  example_eems <- eem_dir_read("data-raw", pattern="SEM|BEM|ManualExampleTea")

  #read in absorbance
  example_abs <- abs_dir_read("data-raw", pattern="ABS[.]dat|SpectraGraphs.dat")

    usethis::use_data(example_eems, overwrite = T)
    usethis::use_data(example_abs, overwrite = T)

  metadata <- meta_read(input="data-raw/metadata_example.csv")
  usethis::use_data(metadata, overwrite = T)


#code to create long term tea and blanks -----
    output_dir <- "data-raw/long-term-standards/blanks"

  #load in to downscale for example data
    #meta <- read.csv(file.path(output_dir, "merged-blk-metadata.csv"))
    #example_dat <- meta$data_identifier[50:55]
    #"2023_01_06_BLK2301061" "2023_01_06_BLK2301062" "2023_01_09_BLK2301091" "2023_01_09_BLK2301092" "2023_02_23_BLK2302231" "2023_02_23_BLK2302232"

    dat_files <- list.files(output_dir, recursive = TRUE)
    for(x in 1:length(example_dat)){
      files <- grep(example_dat[x], dat_files, value=TRUE)

      for(f in files){
        downscale_eems(file.path(output_dir, f))

        #rename
        end <- ifelse(grepl("_blank", f), "BEM", ifelse(grepl("abs/", f), "ABS", "SEM"))
        file.rename(file.path("data-raw", basename(f)),
                    file.path("inst/extdata/long-term-blanks/", paste0("longtermblank", x, end, ".dat")))
      }
    }

    #write metadata
      # blk_meta <- meta[50:55,]
      # blk_meta$data_identifier <- paste0("longtermblank", 1:length(example_dat))
      # write.csv(blk_meta, "inst/extdata/long-term-blanks/longtermblank-metadata.csv", row.names=FALSE, quote=FALSE)

  #make a test mdl file for functions
    create_mdl(file.path(system.file("extdata", package = "eemanalyzeR"), "long-term-blanks"),
                      meta_name="longtermblank-metadata.csv",
                      type="eem", qaqc_dir = system.file("extdata", package = "eemanalyzeR"), "qaqc-stds")

    create_mdl(file.path(system.file("extdata", package = "eemanalyzeR"), "long-term-blanks"),
            meta_name="longtermblank-metadata.csv",
            type="abs", qaqc_dir = system.file("extdata", package = "eemanalyzeR"), "qaqc-stds")

#do the same for the tea samples
    #pull files from Aqualog folder and put in raw data
    output_dir <- "data-raw/long-term-standards/tea-standards"

  #load in to downscale for example data
    meta <- read.csv(file.path(output_dir, "merged-tea-metadata.csv"))
    example_dat <- meta$data_identifier[50:55]
    #"2023_03_24b_postTea230324" "2023_06_12_preTea230612"   "2023_06_12_postTea230612"  "2023_06_12b_preTea230612"  "2023_06_13_preTea230613"  "2023_06_13_postTea230613"

    #write metadata
      # blk_meta <- meta[50:55,]
      # blk_meta$data_identifier <- paste0("longterm-checkstd", 1:length(example_dat))
      # write.csv(blk_meta, "inst/extdata/long-term-std/longterm-checkstd-metadata.csv", row.names=FALSE, quote=FALSE)

    #make a test tea file for functions
    create_std(file.path(system.file("extdata", package = "eemanalyzeR"), "long-term-std"),
            meta_name="longterm-checkstd-metadata.csv", abs_pattern="ABS",
            type="eem", qaqc_dir = system.file("extdata", package = "eemanalyzeR"), "qaqc-stds")

    create_std(file.path(system.file("extdata", package = "eemanalyzeR"), "long-term-std"),
            meta_name="longterm-checkstd-metadata.csv", abs_pattern="ABS",
            type="abs", qaqc_dir = system.file("extdata", package = "eemanalyzeR"), "qaqc-stds")

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

#make files that are fully processed to prevent needing to do all the steps ------
  example_processed_abs <- add_metadata(metadata, example_abs)
  example_processed_abs <- correct_dilution(example_processed_abs)
  example_processed_eems <- add_metadata(metadata, example_eems)
  example_processed_eems <- add_blanks(example_processed_eems, subset_type(example_processed_eems, "iblank"))
  example_processed_eems <- process_eem(example_processed_eems, example_processed_abs)

  usethis::use_data(example_processed_eems, overwrite = T)
  usethis::use_data(example_processed_abs, overwrite = T)

#read yaml and save as default so we can document it ------
  default_config <- yaml::read_yaml(
    file.path(system.file("extdata", package = "eemanalyzeR"),
              "eemanalyzeR-config.yaml"))

  usethis::use_data(default_config, overwrite = TRUE)
