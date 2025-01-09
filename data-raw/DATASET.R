## code to prepare `DATASET` dataset goes here

usethis::use_data(DATASET, overwrite = TRUE)

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
    write_lines(data, file.path("data-raw",file))
  }

  #downscale absorbance
  if(grepl("ABS", file)){
    data <- readLines(file.path("data-raw/fullsize data",file)) #read in file
    data <- data[seq(1, length(data), 6)]
    write_lines(data, file.path("data-raw",file))

  }}

#get full size example files
files <- list.files("data-raw/fullsize data", pattern=".dat")

#downscale and save
for(x in files){
  downscale_eems(x)
}

#read into R environment
  #read in blanks
    files <- list.files("data-raw", pattern="BEM[.]dat")
    for(x in files){
      eemlist <- eemR::eem_read(file.path("data-raw", x), import_function = "aqualog")
      eemlist[[1]]$sample <- gsub("BEM", "", gsub("B1S[1-9]", "",  eemlist[[1]]$sample)) #tidy name
      if(x == files[1]){
        example_blanks <- eemlist
      }else{example_blanks <- eemR::eem_bind(example_blanks, eemlist)}
    }

  #read in samples
    files <- list.files("data-raw", pattern="SEM[.]dat")
    for(x in files){
      eemlist <- eemR::eem_read(file.path("data-raw", x), import_function = "aqualog")
      eemlist[[1]]$sample <- gsub("SEM", "", gsub("B1S[1-9]", "",  eemlist[[1]]$sample)) #tidy name
      if(x == files[1]){
        example_samples <- eemlist
      }else{example_samples <- eemR::eem_bind(example_samples, eemlist)}
    }

  #read in absorbance
    files <- list.files("data-raw", pattern="ABS[.]dat")
    for(x in files){
      abs <- read.table(file.path("data-raw", x))
      colnames(abs) <- c("wavelength", gsub(".dat", "", gsub("ABS", "", gsub("B1S[1-9]", "", x))))
      if(x == files[1]){
        example_absorbance <- abs
      }else{example_absorbance <- cbind(example_absorbance, abs[2])}
    }

    usethis::use_data(example_blanks)
    usethis::use_data(example_samples)
    usethis::use_data(example_absorbance)
