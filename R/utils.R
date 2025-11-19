#' Checks if sample is a blank
#'
#' @param obj an object
#' @noRd
.is_blank <- function(obj) {
  # Get the attribute
  val <- attr(obj, "is_blank$")
  #stopifnot(is.logical(val))
  if(is.null(val)){val <- FALSE}
  return(val)
}

#' Returns eemanalyzeR package version loaded
#'
#' @return text string with eemanalyzeR package version
#' @noRd
.eemanalyzeR_ver <- function() {
  paste0("eemanalyzeR ", utils::packageVersion("eemanalyzeR"))
}

# Overload the bracket operator for eemlist subsetting
#' Subsetting using `[` for eemlist
#'
#' @param eemlist the eemlist to subset
#' @param i the index for subsetting
#'
#' @export
#' @method `[` eemlist
#'
`[.eemlist` <- function(eemlist, i) {
  sublist <- NextMethod()
  structure(sublist, class = "eemlist")
}

#' Write a line of text to the readme object that tracks processing tracking
#'
#' @param text line of text to write to the process file
#' @param slot the spot to write the readme lines into
#' @param args the argument values from upper functions as character
#' @param append append text to existing text in slot?
#' @noRd
.write_readme_line <- function(text, slot, args=NULL, append=FALSE){
  #if this is the first thing getting written to readme, create
  if(!exists("readme")){
    readme <- list(eem_blank_corrected=NA, eem_scatter_corrected=NA,
                   eem_ife_corrected=NA, eem_raman_normalized=NA,
                   eem_doc_normalized=NA, eem_dil_corrected=NA,
                   abs_dil_corrected=NA, abs_doc_normalized=NA,
                   eem_cut=NA,indices=NA, mdl=NA, check_std=NA)
    assign("readme", readme, envir = .GlobalEnv)
  }

  #write processing to readme
  time <- Sys.time()
  time <- strftime(time, format="%Y-%m-%d %H:%M:%S")

  if(!is.null(args)){
    args <- paste("\t",paste0(names(args), ": ", args), collapse="\n")
    args <- paste0(paste("   function parameters:", args, sep="\n"), "\n")
    args <- gsub("~", "", args)

  }else{args <- ""}

  if(append){
    readme[slot] <- paste(readme[slot], text, args, sep="")
  }else{
    step <- paste0(time, ": ", text)
    readme[slot] <- paste(step, args, sep="\n")}

  assign("readme", readme, envir = .GlobalEnv)

}

#' Answer validation questions yes or no
#'
#' @importFrom rlang is_interactive
#' @noRd
.yesorno <- function(question,
                     y_response,
                     n_response) {
  # Return TRUE (ie "yes") if run non-interactively (tests, batch processing)
  if (!rlang::is_interactive()) return(TRUE)
  stopifnot(  is.character(question) |
              is.character(y_response) |
              is.character(n_response))
  cont <- readline(paste0(question, " [y/n]: "))
  if(grepl("^y$", cont, ignore.case = TRUE)) {
    message(y_response, "\n")
    return(TRUE)
  } else if(grepl("^n$", cont, ignore.case = TRUE)){
    message(n_response, "\n")
    return(FALSE)
  } else {
    warning("Improper response, please respond 'y' or 'n'", "\n")
    .yesorno(question,
             y_response,
             n_response)
  }
}


#' Checks if the eems or absorbance has had metadata added
#' @noRd
.meta_added <- function(x){
  stopifnot(class(x) %in% c("eemlist", "eem", "abs", "abslist"))

  if(inherits(x, c("eem","abs"))){
    items <- names(x)
    augment_names <- c("meta_name","dilution","analysis_date", "description","doc_mgL","notes")
    augmented <- all(augment_names %in% items)
   }else{
    augmented <- unlist(lapply(x, .meta_added))
   }
  return(augmented)
}

#' Checks if the eems or absorbance has had blank added
#' @noRd
.blk_added <- function(x){
  stopifnot(class(x) %in% c("eemlist", "eem", "abs", "abslist"))

  if(inherits(x, c("eem","abs"))){
    items <- names(x)
    augment_names <- c("blk_file", "blk_x")
    augmented <- all(augment_names %in% items)
  }else{
    augmented <- unlist(lapply(x, .blk_added))
  }
  return(augmented)

}


#' Removes extra list items from eemlist, replaces sample with meta_name for matching
#' @noRd
.make_base_eem <- function(x){
  if(.meta_added(x)){
    x$sample <- x$meta_name
    x$meta_name <- NULL
    x$dilution <- NULL
    x$integration_time_s <- NULL
    x$raman_area_1s <- NULL
    x$analysis_date <- NULL
    x$description <- NULL
    x$doc_mgL <-NULL
    x$notes <- NULL
    x$blk_x <- NULL
    x$blk_file <- NULL
  }
  return(x)

}


#' Check if two eem matrices are equal
#'
#' @param x1 a matrix "x" from an eem
#' @param x2 a matrix "x" from an eem
#'
#' @noRd
#'
.eem_equal <- function(x1, x2){
  x1_long <- as.vector(x1)
  x2_long <- as.vector(x2)

  equal <- all.equal(x1_long, x2_long)
  equal <- ifelse(equal == TRUE, TRUE, FALSE)
  return(equal)
}


# Overload the bracket operator for abslist subsetting
# we want to always return an abslist

#'Subsetting using `[` for an abslist
#'
#' @param abslist the abslist to subset
#' @param i the index for subsetting
#'
#' @export
#' @method `[` abslist
#'
`[.abslist` <- function(abslist, i) {
  sublist <- NextMethod()
  structure(sublist, class = "abslist")
}

#' Check which processing steps have been completed on an eem or eemlist
#'
#' @param eem an \code{eemlist} or \code{eem} object containing EEM's data.
#'
#' @return a data.frame where the first column is the different attributes and the second is a T/F where F indicates that not all the samples
#' have been processed for that attribute.
#'
check_processing <- function(eem){
  stopifnot(.is_eemlist(eem) | .is_eem(eem))
  steps <- data.frame(attr = c("is_blank_corrected", "is_scatter_corrected","is_ife_corrected",
                               "is_raman_normalized", "is_dil_corrected", "is_doc_normalized"),
                      warning = c("blank corrected", "scattering removed", "ife corrected",
                                  "raman normalized", "dilution corrected", "DOC normalized"),
                      func = c("subtract_blank", "remove_scattering", "ife_correct",
                               "raman_normalized", "correct_dilution", "eem_normalize"),
                      done = FALSE)

  if(.is_eemlist(eem)){
    steps$done <- apply(steps, 1, function(r){done = any(sapply(eem, attr, r[1]))})
  }

  if(.is_eem(eem)){
    steps$done <- apply(steps, 1, function(r){done = attr(eem, r[1])})
  }


  return(steps)
}


#' Nicely combine data QAQC flags
#'
#' If the previous flags were `NA`, replaces with the new flagged value, otherwise combines
#' the flags with a "_" between.
#'
#' @param x existing flags
#' @param x1 flags to add
#' @param mdl logical, combing two MDL flags?
#'
#' @export
#' @examples
#' .combine_flags("DATA01", NA)
#' .combine_flags(NA, "MDL01")
#' .combine_flags(NA, NA)
#' .combine_flags("DATAO1", "MDL01")
#' .combine_flags("DATA01", "DATA01")
.combine_flags <- function(x, x1, mdl=FALSE){
  stopifnot(length(x) == length(x1))

  if(length(x) > 1){
    flags <- sapply(1:length(x), function(n){.combine_flags(x[n], x1[n], mdl=mdl)})
    return(flags)
  }

  if(is.na(x) & is.na(x1)){return(NA)}

  if(mdl & !is.na(x) & !is.na(x1)){
    #if one is full mdl, but another is partial, return MDL03 indicating that one set was totally below
    if(x1 == "MDL01" & x == "MDL02"){x1 <- x <-  "MDL03"}
    if(x == "MDL01" & x1 == "MDL02"){x <- x1 <- "MDL03"}
  }

  #if combining two mdl columns, (should always be a ratio, so if one is NA and the other is MDL01, don't report -> MDL03)
  if(mdl){
    if(is.na(x) & x1 == "MDL01"){x1 <- "MDL03"}
    if(is.na(x1) & x == "MDL01"){x <- "MDL03"}}


  if(is.na(x) & !is.na(x1)){return(x1)}

  if(!is.na(x) & is.na(x1)){return(x)}

  if(!is.na(x) & !is.na(x1)){
    if(x == x1){return(x)
      }else{
      return(paste(x,x1, sep="_"))
    }}
}

#' Just a nicer way to get the directory where the QAQC files should live
#' @noRd
.qaqc_dir <- function(){
  return(file.path(rappdirs::user_data_dir(appname = "eemanalyzeR"), "qaqc-stds"))
}

#' Look for MDL files
#'
#' If they exist will load, if not will warn. Writes the appropriate message about
#' MDL in the readme.
#'
#' @param mdl_dir file path to the mdl files generated with \link[eemanalyzeR]{create_mdl}
#'
#' @noRd
#'
.check_mdl_file <- function(mdl_dir){
  #get mdl data
  check_eem <- file.exists(file.path(mdl_dir, "eem-mdl.rds"))
  check_abs <- file.exists(file.path(mdl_dir, "abs-mdl.rds"))

  #load mdl data or warn
  if(!check_eem){
    warning("fluorescence MDL is missing, indices will not be checked for MDLs")
    .write_readme_line("Fluorescence indices were not checked against method detection limits (MDL)", "mdl")
    eem_mdl <- NULL
  }else{eem_mdl <- readRDS(file.path(mdl_dir, "eem-mdl.rds"))
  .write_readme_line("Fluorescence indices were checked against method detection limits (MDL)", "mdl")
  }

  if(!check_abs){
    warning("absorbance MDL is missing, indices will not be checked for MDLs")
    .write_readme_line("Absorbance indices were not checked against method detection limits (MDL)\n", "mdl", append = TRUE)
    abs_mdl <- NULL
  }else{abs_mdl <- readRDS(file.path(mdl_dir, "abs-mdl.rds"))
  .write_readme_line("Absorbance indices were checked against method detection limits (MDL)\n", "mdl", append=TRUE)
  }

  return(list(eem_mdl = eem_mdl, abs_mdl=abs_mdl))
}
