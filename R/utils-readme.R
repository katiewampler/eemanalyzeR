#' Wrapper to cleanly get the readme from the package enviornment
#'
#' @noRd
#'
get_readme <- function(){.pkgenv[["readme"]]}

set_readme <-function(val){.pkgenv[["readme"]] <- val}

#' Returns eemanalyzeR package version loaded
#'
#' @return text string with eemanalyzeR package version
#' @noRd
.eemanalyzeR_ver <- function() {
  paste0("eemanalyzeR ", utils::packageVersion("eemanalyzeR"))
}

#' Write a line of text to the readme object that tracks processing
#'
#' @param text line of text to write to the process file
#' @param slot the spot to write the readme lines into
#' @param args the argument values from upper functions as character
#' @param append append text to existing text in slot?
#' @noRd
.write_readme_line <- function(text, slot, args=NULL, append=FALSE) {
  # Get the readme from the pacakge environment
  readme <- get_readme()
  #if this is the first thing getting written to readme, create
  if(is.null(readme)){
    readme <- list(eem_add_blank = NA, eem_blank_corrected=NA, eem_scatter_corrected=NA,
                   eem_ife_corrected=NA, eem_raman_normalized=NA,
                   eem_doc_normalized=NA, eem_dil_corrected=NA,
                   abs_dil_corrected=NA, abs_doc_normalized=NA,
                   eem_cut=NA,indices=NA, mdl=NA, check_std=NA)
    assign("readme", readme, pos = .pkgenv)
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

  assign("readme", readme, .pkgenv)

}

#' Print `readme`
#'
#' Nicely print the processing documentation readme file with proper formatting.
#' The readme tracks the processing steps applied to the dataset and is stored
#' as an object called `readme` in the package environment.
#'
#' @export
#'
#' @returns None (invisible NULL).
#' @examples
#' print_readme()
print_readme <- function(){
  readme <- get_readme()[!is.na(get_readme())]

  #get stuff for the top
  date <- strftime(Sys.time(), format="%Y-%m-%d %H:%M")
  version <- paste0("Data processed using ", .eemanalyzeR_ver(), " package in R.")
  link <- "For details on processing steps, indices, and QA/QC flags see the package website: https://katiewampler.github.io/eemanalyzeR/articles/output-documentation.html"

  cat(paste(date, version, link, "______________________________\n", paste(unlist(readme), collapse = "\n"), sep="\n"))

}
