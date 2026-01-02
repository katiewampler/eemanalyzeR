#' Check if a check standard is consistent with long-term standard
#'
#' Calculate indices for a check standard to ensure they are consistent
#' with the long-term standard values.
#'
#' @md
#'
#' @param eemlist An `eemlist` object.
#' @param abslist An `abslist` object.
#' @param qaqc_dir File path to the QAQC files generated with [create_mdl()]
#' and [create_std()]. Default is a user-specific data directory
#' [rappdirs::user_data_dir()].
#' @param tolerance Maximum percent deviation that the check standard can vary
#'   from the long-term values without being flagged.
#' @param index_method Either "eemanalyzeR", "eemR", "usgs", or a custom function.
#' @param vals If `TRUE`, returns actual values, otherwise returns just flags.
#'
#' @seealso [get_indices()]
#' @details
#' `eemlist` and `abslist` must be fully processed to match the long-term standard.
#'
#' @returns
#'
#' If `vals = FALSE`, returns a `data.frame` with four columns:
#'
#' - `meta_name`: metadata name of the check standard
#' - `type`: `abs` or `eem` indicating the index type
#' - `index`: name of the index
#' - `flag`: "STD01" if outside tolerance, otherwise `NA`
#'
#' If `vals = TRUE`, additional columns include:
#'
#' - `observed`: observed index value
#' - `standard`: long-term standard value
#' - `percent_deviation`: percent difference from the standard
#'
#' @export
#'
#' @examples
#' check_std(
#'   example_processed_eems,
#'   example_processed_abs,
#'   qaqc_dir = system.file("extdata", package = "eemanalyzeR")
#' )
check_std <- function(eemlist, abslist, qaqc_dir = .qaqc_dir(), tolerance = 0.2,
                      index_method = "eemanalyzeR", vals = FALSE) {
  stopifnot(is.numeric(tolerance), .is_eemlist(eemlist) | .is_abslist(abslist))

  # check if sample has any check samples if not return empty table
  if (length(subset_type(eemlist, type = "check")) == 0 & length(subset_type(abslist, type = "check")) == 0) {
    warning("No check standard samples found")
    .write_readme_line("No check standards were included in the sample data\n", "check_std")

    return(data.frame(meta_name = rep("notea", each = 2), index = NA, type = c("abs", "eem"), flag = NA))
  }

  # get eem_std and abs_std
  if (is.null(qaqc_dir) || (!file.exists(file.path(qaqc_dir, "eem-check-std.rds")) | !file.exists(file.path(qaqc_dir, "abs-check-std.rds")))) {
    warning("tea check standard files are missing, check standards will not be checked against the long-term standard")
    .write_readme_line("Long-term standards were not provided, thus the check standards for this run were not checked\n", "check_std")

    names <- get_sample_info(subset_type(eemlist, type = "check"), "meta_name")
    return(data.frame(meta_name = rep(names, each = 2), index = NA, type = c("abs", "eem"), flag = NA))
  } else {
    eem_std <- readRDS(file.path(qaqc_dir, "eem-check-std.rds"))
    abs_std <- readRDS(file.path(qaqc_dir, "abs-check-std.rds"))
  }

  # get attributes to make sure they're processed the same as the standard
  if (any(check_processing(eemlist)$done != check_processing(eem_std)$done)) {
    stop("processing steps are different between check standard and eemlist")
  }

  # get index function
  index_function <- get_indices_function(index_method)

  # calculate indices for standard
  # make std a length 1 eemlist/abslist
  eem_std <- list(eem_std)
  class(eem_std) <- "eemlist"

  abs_std <- list(abs_std)
  class(abs_std) <- "abslist"

  std_index <- index_function(eem_std, abs_std, qaqc_dir = qaqc_dir)

  # calculate indices for tea
  tea_index <- index_function(subset_type(eemlist, type = "check"),
    subset_type(abslist, type = "check"),
    qaqc_dir = qaqc_dir
  )

  # tidy and combine indices
  if (inherits(std_index[[1]], "data.frame") & inherits(tea_index[[1]], "data.frame")) {
    abs_ind <- merge(tea_index$abs_index, std_index$abs_index %>% select(any_of(c("index", "value"))) %>%
      dplyr::rename(std_val = "value"), by = "index") %>% mutate(type = "abs")
  } else {
    abs_ind <- data.frame(
      index = character(), sample_name = character(), meta_name = character(),
      value = numeric(), std_val = numeric(), type = character
    )
  }

  if (inherits(std_index[[2]], "data.frame") & inherits(tea_index[[2]], "data.frame")) {
    eem_ind <- merge(tea_index$eem_index, std_index$eem_index %>% select(any_of(c("index", "value"))) %>%
      dplyr::rename(std_val = "value"), by = "index") %>% mutate(type = "eem")
  } else {
    eem_ind <- data.frame(
      index = character(), sample_name = character(), meta_name = character(),
      value = numeric(), std_val = numeric(), type = character
    )
  }

  indices <- rbind(eem_ind, abs_ind)

  # determine how many are within threshold
  if (nrow(indices) > 0) {
    # get non flagged values
    indices <- indices[!(grepl("[A-Z]{3,}[0-9]{1,}", indices$value) | grepl("[A-Z]{3,}[0-9]{1,}", indices$std_val)), ]

    # calc percent different
    indices$per_dif <- abs(as.numeric(indices$std_val) - as.numeric(indices$value)) / as.numeric(indices$std_val)

    # get flag
    indices$flag <- ifelse(indices$per_dif > tolerance, "STD01", NA)

    # format nice for returning
    report <- indices %>% select(any_of(c("meta_name", "type", "index", "flag")))

    # calculate percentage that are outside threshold (per sample??) -> mark tea only??? -> similar to blanks
    sum <- indices %>%
      mutate(flag = ifelse(.data$per_dif > tolerance, TRUE, FALSE)) %>%
      dplyr::group_by(.data$type) %>%
      dplyr::summarise(per_out = sum(.data$flag) / dplyr::n(), .groups = "keep")

    # write to the readme
    tea_msg <- paste(paste0(round(sum$per_out[1], 2) * 100, "% (n=", sum(indices$type == "abs"), ") of the absorbance indices were greater than ", tolerance * 100, "% of the long-term check standard"),
      paste0(round(sum$per_out[2], 2) * 100, "% (n=", sum(indices$type == "eem"), ") of the fluorescence indices were greater than ", tolerance * 100, "% of the long-term check standard\n"),
      sep = "\n"
    )
    .write_readme_line(tea_msg, "check_std")

    # return the things requested
    if (vals) {
      return(as.data.frame(indices) %>% select(-"sample_name"))
    }

    return(as.data.frame(report))
  } else {
    names <- get_sample_info(subset_type(eemlist, type = "check"), "meta_name")
    return(data.frame(meta_name = rep(names, each = 2), index = NA, type = c("abs", "eem"), flag = NA))
  }
}
