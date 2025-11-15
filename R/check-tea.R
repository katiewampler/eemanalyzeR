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
#' return a table with the observed and mdl values for the ex/em pair
#' @md
#' @returns
#' #TODO: update returns values
#' If `vals` is FALSE:
#'  - STD01 if all values outside of the tolerance
#'  - STD02 if some of the values are outside of the tolerance
#'  - NA if all are within the tolerance
#'
#' If `vals` is TRUE:
#'  - a `data.frame` with the index, the observed value, the long-term standard value, and the percent deviation
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
    return(NULL)

    warning("fluorescence MDL is missing, indices will not be checked for MDLs")
    .write_readme_line("Fluorescence indices were not checked against method detection limits (MDL)", "mdl")
  }else{
    eem_std <- readRDS(file.path(std_dir,"eem-tea-std.rds"))
    abs_std <- readRDS(file.path(std_dir, "abs-tea-std.rds"))
  }

    #TODO: if file doesn't exist
      #add note in readme -> return NA

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

    std_index <- index_function(eem_std, abs_std)

  #calculate indices for tea
    tea_index <- index_function(subset_qaqc(eemlist, type="tea_std"), subset_qaqc(abslist, type="tea_std"))

  #tidy and combine indices
    abs_ind <- merge(tea_index$abs_index, std_index$abs_index %>% select(any_of(c("index", "value"))) %>%
                       dplyr::rename(std_val = "value"), by="index") %>% mutate(type = "abs")

    eem_ind <- merge(tea_index$eem_index, std_index$eem_index %>% select(any_of(c("index", "value"))) %>%
                       dplyr::rename(std_val = "value"), by="index") %>% mutate(type="eem")

    indices <- rbind(eem_ind, abs_ind)

  #determine how many are within threshold
    #get non flagged values
    indices <- indices[!(grepl("[A-Z]{3,}[0-9]{1,}", indices$value) |  grepl("[A-Z]{3,}[0-9]{1,}", indices$std_val)),]

    #calc percent different
    indices$per_dif <- abs(as.numeric(indices$std_val) - as.numeric(indices$value)) / as.numeric(indices$std_val)

  #calculate percentage that are outside threshold (per sample??) -> mark tea only??? -> similar to blanks
    report <- indices %>% mutate(tea_flag = ifelse(.data$per_dif > tolerance, TRUE, FALSE)) %>%
      dplyr::group_by(.data$meta_name, .data$type) %>% dplyr::summarise(per_out = sum(.data$tea_flag)/ dplyr::n(),
                                                                        tea_flag = any(.data$tea_flag))


    if(vals){
      return(indices)
    }

    #TODO: convert T/F to correct flag, write to the readme if any flags
    return(report)
  #in outer function ->
    #print to readme (add a spot for tea check), if not checked mark that too
    #note to flag indices if readme isn't NULL/not checked
}
