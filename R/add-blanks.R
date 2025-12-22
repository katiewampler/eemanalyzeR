#' Check blanks and add to sample data
#'
#' Allows the user to visually check excitation-emission matrix (EEM) plots of the blanks,
#' and then, if approved, the blanks will be added to the `eem` objects for further processing.
#'
#' @md
#'
#' @details
#' If more than one blank is supplied, the function links each blank to its sample using the
#' metadata name. These names must match between the sample and its corresponding blank.
#' Because of this, samples must already have metadata assigned using
#' [add_metadata()].
#'
#' If a `blanklist` is not provided, one is automatically generated from the `eemlist` attribute
#' "sample_type", using the samples marked as "iblank".
#'
#' @note
#' If the instrument blank is not accepted, the function will attempt to use an
#' analytical blank instead (a sample type 'sblank'). If this blank is used instead,
#' the blank will be removed from the sample set and a note will be written to the readme.
#'
#' @param eemlist An `eemlist` object.
#' @param blanklist Optional. An `eem` or `eemlist` containing the blank EEM(s).
#'
#' @return
#' An `eemlist` where each `eem` object has two added components:
#'
#' - `blk_file`: file path of the blank associated with the sample
#' - `blk_x`: the blank EEM associated with the sample
#'
#' @export
#'
#' @examples
#' eemlist <- add_metadata(metadata, example_eems)
#' blanklist <- subset_type(eemlist, "iblank")
#' augment_eemlist <- add_blanks(eemlist, blanklist)
add_blanks <- function(eemlist,
                       blanklist) {
  stopifnot(
    "Invalid eemlist provided." = class(eemlist) %in% c("eemlist"),
    "Invalid blanklist provided: blanklist is not an eem or eemlist." = class(blanklist) %in% c("eem", "eemlist")
  )

  # Remove the blanks in blanklist from the eemslist
  nm_blk <- get_sample_info(blanklist, "sample")
  nm_eem <- get_sample_info(eemlist,   "sample")
  eemlist <- eemlist[-match(nm_blk, nm_eem)]

  # convert a single eem to eemlist for simplicity
  if (inherits(blanklist, "eem")) {
    blanklist <- list(blanklist)
    class(blanklist) <- "eemlist"
  }

  if (length(blanklist) == 0 | length(eemlist) == 0) {
    stop("eemlist or blanklist had zero samples, please check that the blk argument is correctly marking blanks in eemanalyzeR::eem_dir_read()")
  }
  # make sure there's metadata added to samples
  if (!any(.meta_added(eemlist)) | !any(.meta_added(blanklist))) {
    stop("metadata must be added to link the samples and blanks, please run 'eemanalyzeR::add_metadata' first")
  }

  # makes sure blank has same wavelengths as sample then adds into eem as x_blK
  .add_x_blk <- function(eem, eem_blk) {
    if (!identical(eem$ex, eem_blk$ex) | !identical(eem$em, eem_blk$em)) {
      stop("excitation and/or emission wavelengths as mismatched between sample and blank")
    }

    if(.meta_added(eem) & .meta_added(eem_blk) && eem$integration_time_s != eem_blk$integration_time_s){
      stop("integration times are not the same between the sample and the blank")

    }
    eem$blk_file <- eem_blk$file
    eem$blk_x <- eem_blk$x
    rownames(eem$blk_x) <- eem_blk$em
    colnames(eem$blk_x) <- eem_blk$ex
    class(eem) <- "eem"
    return(eem)
  }

  if (length(blanklist) == 1) {
    # if only one eem, add to all eems
    eemlist <- lapply(eemlist, .add_x_blk, blanklist[[1]])
    class(eemlist) <- "eemlist"
  } else if (length(blanklist) == length(eemlist)) {
    # if same as eemlist try to match
    eem_names <- get_sample_info(eemlist, "meta_name")
    blank_names <- get_sample_info(blanklist, "meta_name")
    if (length(setdiff(eem_names, blank_names)) > 0) {
      stop("more than one blank was provided, but blank names do not match samples")
    } else {
      blanklist <- blanklist[match(eem_names, blank_names)] # make sure blanklist is in same order as eemlist
      eemlist <- mapply(.add_x_blk, eemlist, blanklist, SIMPLIFY = F)
      class(eemlist) <- "eemlist"
    }
  } else {
    # if not, stop and give error
    browser()
    stop("more than one blank was provided, but blank names do not match samples")
  }


  return(eemlist)
}