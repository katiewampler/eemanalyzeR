#' Check blanks and add to sample data
#'
#' Allows user to visually check excitation emissions matrix plots of the blanks, and then, if approved, the blanks will
#' be added to the \code{eem} objects for use in further processing.
#'
#' @details
#' If more than one blank is supplied, the function links the blank and sample by the metadata
#' name which should be the same between the sample and the blank, because of this,
#' samples must have metadata already added to the samples using \link[eemanalyzeR]{add_metadata}.
#'
#' Adding the blanks into the sample data can be done two ways:
#' \enumerate{
#'  \item Include an \code{eemlist} containing EEM's data for both sample and blank: Need to specify a \code{pattern} to identify blanks
#'  \item Include an \code{eemlist} containing \strong{only} EEM's data for samples: Need to include an \code{eem} or \code{eemlist} as \code{blanklist} with EEM's data for blank(s)
#' }
#'
#' @param eemlist an \code{eemlist} object containing EEM's data. See details for more info.
#' @param blanklist optional. an \code{eem} or \code{eemlist} containing the EEM's data for the blank(s)
#' @param pattern optional. a character string containing a \code{\link[base]{regular expression}}
#' used to specify the names of the samples to be used for blanks.
#'
#' @returns an \code{eemlist} where each \code{eem} object has two additional items:
#' \itemize{
#' \item \code{blk_file}: the file location for the blank associated with the sample
#' \item \code{blk_x}: the blank EEM associated with the sample
#' }
#' @export
#'
#' @importFrom staRdom ggeem
#' @examples
#' eemlist <- add_metadata(metadata, example_eems)
#' augment_eemlist <- add_blanks(eemlist)
#'



#to do: add blank file location to eem
add_blanks <- function(eemlist, blanklist=NULL, pattern="BEM|Blank$"){
  stopifnot(class(eemlist) %in% c("eemlist"),
            class(blanklist) %in% c("NULL", "eem", "eemlist"),
            is.character(pattern))

  #convert a single eem to eemlist for simplicity
  if(inherits(blanklist, "eem")){
    blanklist <- list(blanklist)
    class(blanklist) <- "eemlist"
  }

  #if no blanks are provided
  if(is.null(blanklist)){
      #separate into blanklist and eemlist based on pattern given
      blanklist <- eem_get_blank(eemlist, pattern=pattern, info="sample")
      eemlist <- eem_rm_blank(eemlist, pattern=pattern, info="sample")
  }

  if(length(blanklist) == 0 | length(eemlist) == 0){
    stop("eemlist or blanklist had zero samples, please ensure 'pattern' argument is correct")
  }

  #make sure there's metadata added to samples
    if(!any(.meta_added(eemlist) & .meta_added(blanklist)) & length(blanklist) > 1){
      stop("metadata must be added to link the samples and blanks, please run 'eemanalyzeR::add_metadata' first")
    }

  #only plot unique blanks (replace with my plotting function once written)
  plot_check <- staRdom::ggeem(unique(blanklist))

  #only ask if in an interactive session
  if(interactive()){
    #return to user
    print(plot_check)

    #ask user if processing should continue
    continue <- .yesorno("After reviewing blank(s), do you want to continue processing samples", "","")

  }else{
    continue <- TRUE
  }

  #makes sure blank has same wavelengths as sample then adds into eem as x_blK

  .add_x_blk <- function(eem, eem_blk){
    if(!identical(eem$ex, eem_blk$ex) | !identical(eem$em, eem_blk$em)){
      stop("excitation and/or emission wavelengths as mismatched between sample and blank")
    }
    eem$blk_file <- eem_blk$file
    eem$blk_x <- eem_blk$x
    class(eem) <- "eem"
    return(eem)
  }
  if(continue){
    if(length(blanklist) == 1){
      #if only one eem, add to all eems
      eemlist <- lapply(eemlist, .add_x_blk, blanklist[[1]])
      class(eemlist) <- "eemlist"

    }else if(length(blanklist) == length(eemlist)){


      #if same as eemlist try to match
      eem_names <- get_sample_info(eemlist, "meta_name")
      blank_names <- get_sample_info(blanklist, "meta_name")

      if(length(setdiff(eem_names, blank_names)) > 0){
        stop("more than one blank was provided, but blank names do not match samples")
      }else{
        blanklist <- blanklist[match(eem_names, blank_names)] #make sure blanklist is in same order as eemlist

        eemlist <- mapply(.add_x_blk, eemlist, blanklist, SIMPLIFY=F)
        class(eemlist) <- "eemlist"
      }
    }else{
      #if not, stop and give error
      stop("more than one blank was provided, but blank names do not match samples")
    }

  }else{
    stop("Processing stopped by user \nto use an alternative blank, provide a different blank via the 'blanklist' argument")
  }

  return(eemlist)
}
