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
    readr::write_lines(data, file.path("data-raw",file))
  }

  #downscale absorbance
  if(grepl("ABS", file)){
    data <- readLines(file.path("data-raw/fullsize data",file)) #read in file
    data <- data[seq(1, length(data), 6)]
    readr::write_lines(data, file.path("data-raw",file))

  }}

#get full size example files
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


  #average is too narrow??/ normalize to max??
#code to create averaged blank
  input_dir <- "data-raw/long term standards"
  avg_blank <- function(input_dir){
    blank_eems <- eem_dir_read(file.path(input_dir, "EEM/blanks"), pattern="blank")  #this takes a little while because there's a lot of samples

    #make all the save wavelengths
    blank_eems_rd <- staRdom::eem_red2smallest(blank_eems)

    #convert to raster to plot
    flat_X <- lapply(lapply(blank_eems_rd, `[[`, 3), as.vector)

    #get unique values, there's likely some duplicates
    flat_X_unique <- flat_X[!duplicated(lapply(flat_X, sort))]

    X_df <- do.call(cbind.data.frame, flat_X_unique)
    colnames(X_df) <- paste0("blk_", 1:ncol(X_df))
    X_df$val <- 1:nrow(X_df)

    X_df_long <- X_df %>% tidyr::pivot_longer(!val, values_to = "fluor", names_to="sample") #!!need to make flexible


    ggplot2::ggplot(X_df_long, ggplot2::aes(x=val, y=log(fluor), group=sample), color="black") + ggplot2::geom_line() #visualize to check for outliers

    #average across all eem's to get a average blank eem
    eem_list <- lapply(blank_eems_rd, `[[`, 3)
    eem_list <- lapply(eem_list, function(x) ifelse(x < 0, NA, x)) #remove negatives
    avg_blank <- purrr::reduce(eem_list,`+`) / length(eem_list)

    avg_blank_flat <- data.frame(val= 1:nrow(X_df), fluor=as.vector(avg_blank))
    ggplot2::ggplot() + ggplot2::geom_line(data=X_df_long, ggplot2::aes(x=val, y=fluor, group=sample), color="black") +
      ggplot2::geom_line(data=avg_blank_flat, ggplot2::aes(x=val, y=fluor), color="red") #visualize to check for outliers

    avg_blank <- list(file="/data-raw/long term standards/EEM/blanks/avg_blank.dat",
                      sample = "average_blank",
                      x = avg_blank,
                      em = blank_eems_rd[[1]]$em,
                      ex = blank_eems_rd[[1]]$ex,
                      location = "data-raw")

    class(avg_blank) <- "eem"

    staRdom::ggeem(avg_blank)
    return(avg_blank)

  }

  longterm_blank <- avg_blank(input_dir)
  usethis::use_data(longterm_blank, overwrite = T)

  #save index ranges as data.frame
  indice_ranges <- read.csv("data-raw/indice-ranges.csv")
  indice_ranges$low_val[indice_ranges$low_val == -9999] <- NA
  indice_ranges$high_val[indice_ranges$high_val == -9999] <- NA
  indice_ranges$sources[indice_ranges$sources == "N/A"] <- NA

  usethis::use_data(indice_ranges, overwrite = T)
