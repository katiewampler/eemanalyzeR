#' Check if a tea standard is consistent with long-term standard
#'
#' Calculate the indices in a tea standard to ensure they are consistent
#' with the long-term standard.
#'
#' @param eemlist an object of class `eemlist`, needs to be fully processed
#' @param abslist an object of class `abslist`, needs to be fully processed
#' @param std_dir file path to the QAQC files generated with \link[eemanalyzeR]{create_mdl} and \link[eemanalyzeR]{create_tea_std},
#' default is a user-specific data directory (\link[rappdirs]{user_data_dir})
#' @param tolerance what is the maximum percentage the tea standard can vary from the long-term values without being flagged?
#' @param index_method the index method used to calculate indices, see \link[eemanalyzeR]{get_indices} for more info.
#' @param vals logical, if FALSE will return a flag, if TRUE will
#' return a table with the observed and standard values for each index.
#' @md
#' @returns
#' A `data.frame` with four columns:
#'  - meta_name: the metadata name of the tea sample
#'  - type: either abs or eem to specify the index type
#'  - index: the name of the index
#'  - tea_flag: a flag indicating if the index outside the tolerance (STD01 or NA).
#'
#' If `vals` is TRUE, the table will also include the observed value, the long-term standard value, and the percent deviation
#'
#' @export
#' @examples
#' check_tea_std(example_processed_eems, example_processed_abs,
#'               std_dir = system.file("extdata", package = "eemanalyzeR"))

check_tea_std <- function(eemlist, abslist, std_dir=.qaqc_dir(), tolerance=0.2,
                          index_method="eemanalyzeR", vals = FALSE){

  stopifnot(is.numeric(tolerance),.is_eemlist(eemlist)|.is_abslist(abslist))

  #get eem_std and abs_std
    if(!file.exists(file.path(std_dir,"eem-tea-std.rds"))|!file.exists(file.path(std_dir, "abs-tea-std.rds"))){
      names <- get_sample_info(subset_qaqc(eemlist, type="tea_std"), "meta_name")
       return(data.frame(meta_name=rep(names, each=2), index=NA, type=c("abs", "eem"), tea_flag=NA))

      warning("tea check standard files are missing, check standards will not be checked against the long-term standard")
      .write_readme_line("Tea standards were non provided, thus the tea standards for this run were not checked", "check_std")
    }else{
      eem_std <- readRDS(file.path(std_dir,"eem-tea-std.rds"))
      abs_std <- readRDS(file.path(std_dir, "abs-tea-std.rds"))
    }

  #get attributes to make sure they're processed the same as the standard
    if(any(check_processing(eemlist)$done != check_processing(eem_std)$done)){
      stop("processing steps are different between tea standard and eemlist")
    }

  #get index function
    index_function <- get_indices_function(index_method)

  #calculate indices for standard
    #make std a length 1 eemlist/abslist
    eem_std <- list(eem_std)
    class(eem_std) <- "eemlist"

    abs_std <- list(abs_std)
    class(abs_std) <- "abslist"

    std_index <- index_function(eem_std, abs_std, mdl_dir = std_dir)

  #calculate indices for tea
    tea_index <- index_function(subset_qaqc(eemlist, type="tea_std"),
                                subset_qaqc(abslist, type="tea_std"),
                                mdl_dir = std_dir)

  #tidy and combine indices
    if(inherits(std_index[[1]], "data.frame") & inherits(tea_index[[1]], "data.frame")){
      abs_ind <- merge(tea_index$abs_index, std_index$abs_index %>% select(any_of(c("index", "value"))) %>%
                         dplyr::rename(std_val = "value"), by="index") %>% mutate(type = "abs")
    }else{
      abs_ind <- data.frame(index=character(), sample_name=character(), meta_name=character(),
                        value=numeric(), std_val=numeric(), type=character)
    }

    if(inherits(std_index[[2]], "data.frame") & inherits(tea_index[[2]], "data.frame")){
      eem_ind <- merge(tea_index$eem_index, std_index$eem_index %>% select(any_of(c("index", "value"))) %>%
                         dplyr::rename(std_val = "value"), by="index") %>% mutate(type="eem")
    }else{
      eem_ind <- data.frame(index=character(), sample_name=character(), meta_name=character(),
                            value=numeric(), std_val=numeric(), type=character)
    }

    indices <- rbind(eem_ind, abs_ind)

  #determine how many are within threshold
  if(nrow(indices) > 0){
    #get non flagged values
    indices <- indices[!(grepl("[A-Z]{3,}[0-9]{1,}", indices$value) |  grepl("[A-Z]{3,}[0-9]{1,}", indices$std_val)),]

    #calc percent different
    indices$per_dif <- abs(as.numeric(indices$std_val) - as.numeric(indices$value)) / as.numeric(indices$std_val)

    #get flag
    indices$tea_flag <- ifelse(indices$per_dif > tolerance, "STD01", NA)

    #format nice for returning
    report <- indices %>% select(any_of(c("meta_name", "type", "index", "tea_flag")))

    #calculate percentage that are outside threshold (per sample??) -> mark tea only??? -> similar to blanks
    sum <- indices %>% mutate(tea_flag = ifelse(.data$per_dif > tolerance, TRUE, FALSE)) %>%
      dplyr::group_by(.data$type) %>% dplyr::summarise(per_out = sum(.data$tea_flag)/ dplyr::n(), .groups="keep")

    #write to the readme
    tea_msg <- paste(paste0(sum$per_out[1], "% (n=", sum(indices$type == "abs")  ,") of the absorbance indices were greater than ", tolerance*100, "% of the long-term check standard"),
                     paste0(sum$per_out[2], "% (n=", sum(indices$type == "eem")  ,") of the fluorescence indices were greater than ", tolerance*100, "% of the long-term check standard"), sep="\n")
    .write_readme_line(tea_msg, "check_std")

    #return the things requested
    if(vals){
      return(as.data.frame(indices) %>% select(-"sample_name"))
    }

    return(as.data.frame(report))
  }else{
    names <- get_sample_info(subset_qaqc(eemlist, type="tea_std"), "meta_name")
    return(data.frame(meta_name=rep(names, each=2), index=NA, type=c("abs", "eem"), tea_flag=NA))

  }



}
