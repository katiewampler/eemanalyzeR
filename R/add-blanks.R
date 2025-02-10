#' Check blanks and add to sample data
#'
#' Allows user to visually check excitation emissions matrix plots of the blanks, and then, if approved, the blanks will
#' be added to the \code{eem} objects for use in further processing.
#'
#' @details
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
#' @returns an \code{eemlist} where each \code{eem} object has an additional item: 'x_blk' which is the blank associated with each sample
#' @export
#'
#' @importFrom staRdom ggeem
#' @examples
#' \dontrun{
#' #' eemlist <- add_metadata(metadata, example_eems)
#' augment_eemlist <- add_blanks(eemlist)
#' }


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

  #only plot unique blanks (replace with my plotting function once written)
  plot_check <- staRdom::ggeem(unique(blanklist))

  #return to user
  print(plot_check)

  #ask user if processing should continue
  continue <- readline(prompt = "After reviewing blank(s), do you want to continue processing samples (Y/N): ")

  while(!(toupper(continue) %in% c("Y", "N"))){
    continue <- readline(prompt = "After reviewing blank(s), do you want to continue processing samples (Y/N): ")
  }

  #makes sure blank has same wavelengths as sample then adds into eem as x_blK

  .add_x_blk <- function(eem, eem_blk){
    if(any(eem$em != eem_blk$em) | any(eem$ex != eem_blk$ex)){
      stop("excitation and/or emission wavelengths as mismatched between sample and blank")
    }
    eem$x_blk <- eem_blk$x
    class(eem) <- "eem"
    return(eem)
  }
  if(toupper(continue) == "Y"){
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
    stop("Processing stopped by user \nif you want to use an alternative blank please use 'argument here' to replace the existing blank")
  }

  return(eemlist)
}
