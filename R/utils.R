
# Overload the bracket operator for eemlist subsetting
#' Subsetting using `[` for eemlist
#'
#' @param eemlist the eemlist to subset
#' @param i the index for subsetting
#'
#' @export
#' @method [ eemlist
#'
`[.eemlist` <- function(eemlist, i) {
  sublist <- NextMethod()
  structure(sublist, class = "eemlist")
}

# Overload the bracket operator for abslist subsetting
# we want to always return an abslist

#'Subsetting using `[` for an abslist
#'
#' @param abslist the abslist to subset
#' @param i the index for subsetting
#'
#' @export
#' @method [ abslist
#'
`[.abslist` <- function(abslist, i) {
  sublist <- NextMethod()
  structure(sublist, class = "abslist")
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


#' Just a nicer way to get the directory where the QAQC files should live
#' @noRd
.qaqc_dir <- function(){
  return(file.path(fs::path_norm(rappdirs::user_data_dir(appname = "eemanalyzeR")), "qaqc-stds"))
}

#' Look for MDL files
#'
#' If they exist will load, if not will warn. Writes the appropriate message about
#' MDL in the readme.
#'
#' @param qaqc_dir file path to the mdl files generated with \link[eemanalyzeR]{create_mdl}
#'
#' @noRd
#'
.check_mdl_file <- function(qaqc_dir){
  #get mdl data
  check_eem <- file.exists(file.path(qaqc_dir, "eem-mdl.rds"))
  check_abs <- file.exists(file.path(qaqc_dir, "abs-mdl.rds"))

  #load mdl data or warn
  if(!check_eem){
    warning("fluorescence MDL is missing, indices will not be checked for MDLs")
    .write_readme_line("Fluorescence indices were not checked against method detection limits (MDL)", "mdl")
    eem_mdl <- NULL
  }else{eem_mdl <- readRDS(file.path(qaqc_dir, "eem-mdl.rds"))
  .write_readme_line("Fluorescence indices were checked against method detection limits (MDL)", "mdl")
  }

  if(!check_abs){
    warning("absorbance MDL is missing, indices will not be checked for MDLs")
    .write_readme_line("Absorbance indices were not checked against method detection limits (MDL)\n", "mdl", append = TRUE)
    abs_mdl <- NULL
  }else{abs_mdl <- readRDS(file.path(qaqc_dir, "abs-mdl.rds"))
  .write_readme_line("Absorbance indices were checked against method detection limits (MDL)\n", "mdl", append=TRUE)
  }

  return(list(eem_mdl = eem_mdl, abs_mdl=abs_mdl))
}


