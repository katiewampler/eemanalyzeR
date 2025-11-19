#' Run tests to validate metadata
#'
#' Checks for missing information, ensures column formats are correct to ensure smooth processing
#'
#' @param meta a data.frame with metadata
#' @importFrom lubridate parse_date_time
#' @returns a \code{data.frame} (meta), with errors fixed
#'
#' @examples
#' metadata <- meta_read(system.file("extdata", package = "eemanalyzeR"))
#' metadata <- eemanalyzeR:::meta_check(metadata)


# TODO These might need to be more robust to weird inputs from the user. Worth checking
# a few more edge cases? Can do after we publish package
meta_check <- function(meta){
  #check required columns are there
  req_cols <- c("data_identifier", "replicate_no", "integration_time_s","run_type", "RSU_area_1s",
                "dilution")
  missing_cols <- setdiff(req_cols, colnames(meta))
  if(length(missing_cols) > 0) {
    stop("metadata is missing required column(s): ", paste(missing_cols, collapse = ", "))
  }

  #ensure integration time and RSU adjust area (and DOC/dilution are numeric)
  meta <- meta %>% dplyr::mutate(dplyr::across(dplyr::any_of(c("integration_time_s","RSU_area_1s", "dilution", "DOC_mg_L")), as.numeric))

  #ensure dates are dates
  conv_dates <- unlist(lapply(meta, class))
  meta <- meta %>% dplyr::mutate(dplyr::across(dplyr::any_of(c("analysis_date", "collect_date")), \(x) lubridate::parse_date_time(x, tz=Sys.timezone(), orders=c("ymd", "mdy"))))

  #ensure data_identifier isn't missing data
  if(any(is.na(meta$data_identifier) | is.character(meta$data_identifier) == F)){
    stop("metadata missing data identifiers for one or more samples, \nsee help(eemanalyzeR::metadata) for more info")
  }

  #make sure there aren't any duplicated samples
  unique_ID <- paste(meta$data_identifier, meta$replicate_no, meta$integration_time_s, sep="_")
  if(any(duplicated(unique_ID))){
    stop("duplicate samples found with the same data_identifier, replicate_no, and integration time:\n", paste(unique(unique_ID[duplicated(unique_ID)]), sep="\n"), "\nif they are indeed duplicates, please use 'replicate_no' to note this by using 1,2,3...")
  }

  #ensure RSU_area_1s isn't missing data
  if(any(is.na(meta$RSU_area_1s) | is.numeric(meta$RSU_area_1s) == F)){
    stop("metadata missing values for RSU adjust area, \nsee help(eemanalyzeR::metadata) for more info")
  }

  #check dilution isn't missing or 0, if it is assume it's 1 with a warning
  if(any(meta$dilution == 0 | is.na(meta$dilution))){
    meta$dilution[meta$dilution == 0 | is.na(meta$dilution)] <- 1
    warning("dilutions were missing or set to 0. assuming a value of 1 for 'dilution' which indicates no dilution")
  }

  #check replicate numbers aren't missing, if it is assume it's 1 with a warning
  if(any(is.na(meta$replicate_no))){
    meta$replicate_no[is.na(meta$replicate_no)] <- 1
    warning("replicate numbers were missing. assuming a value of 1 for 'replicate_no' which indicates no replicates")
  }

  #ensure run_type is either sampleQ or manual (might not matter anymore??)
  if(any(grepl("sampleQ|manual", meta$run_type, ignore.case = T)==F)){
    stop("'run_type' must be either 'sampleQ' or 'manual'")
  }

  # Check samples are flagged as blanks or check standards
  if(!is.logical(meta$is_blank) || any(is.na(meta$is_blank))) {
    warning("is_blank column not properly specified. It should contain values TRUE or FALSE\neemanalyzeR will attempt to assign blanks from filenames")
  }
  if(!is.logical(meta$is_check) || any(is.na(meta$is_check))) {
    warning("is_check column not properly specified. It should contain values TRUE or FALSE\neemanalyzeR will attempt to assign check standards from filenames")
  }


  return(meta)
}
