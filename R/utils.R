
# Overload the bracket operator for eemlist subsetting
#' Subsetting using `[` for eemlist
#'
#' @param eemlist the eemlist to subset
#' @param i the index for subsetting
#'
#' @export
#' @keywords internal
#' @md
#' @returns an object of class `eemlist`
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
#' @keywords internal
#' @md
#' @returns an object of class `abslist`
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
.default_config_dir <- function(){
  return(file.path(fs::path_norm(rappdirs::user_data_dir(appname = "eemanalyzeR"))))
}

#' Asks to update QAQC directory in config
#'
#' If `qaqc_dir` is `NA` in the user config file, this tells the code to not check for QAQC tests. However, we want to
#' automatically set the code to use the created QAQC standards. This will ask to update the user_config file, if `Y`,
#' it will write the default storage location for the standards.
#'
#' @returns Result of user input as `TRUE` or `FALSE`.
#'
#' @noRd
update_qaqc_dir <- function(){
  #get paths
    user_dir <- .default_config_dir()
    defaults_file <- file.path(user_dir, "user-config.yaml")
    new_dir <- file.path(user_dir, "qaqc-stds")

  #see what is in file
  if(file.exists(defaults_file)){
    old_qaqc_dir <- yaml::read_yaml(defaults_file)$qaqc_dir
  }

  #only ask if it would change what's there
  if(!file.exists(defaults_file) || is.na(old_qaqc_dir) || old_qaqc_dir != new_dir){
    if(!rlang::is_interactive()){update_path <- TRUE}else{
      update_path <- .yesorno("Update user config file with QAQC file path?",
                              paste0("qaqc_dir in user config has been updated to ", normalizePath(file.path(rappdirs::user_data_dir(appname = "eemanalyzeR"), "qaqc-stds"))),
                              "Warning: qaqc_dir must be manually specified using `qaqc_dir` to use QAQC files in processing.")
    }

    if(update_path){
      if (!dir.exists(new_dir)) dir.create(new_dir, recursive = TRUE, showWarnings = FALSE)

      # if file doesn't exist, write template
      if (!file.exists(defaults_file)) {
        file.copy(file.path(system.file("extdata", package = "eemanalyzeR"), "eemanalyzeR-config.yaml"),
                  defaults_file)}

        new_dir <- normalizePath(new_dir, winslash = "/")

        modify_config(qaqc_dir = new_dir)
        user_config <- readLines(defaults_file)
        user_config[grepl("qaqc_dir:", user_config)] <- paste0('  qaqc_dir: "', new_dir,'"')
        writeLines(user_config, defaults_file)
        }

    }


    return(get_qaqc_dir())


}

#' Look for MDL files
#'
#' If they exist will load, if not will warn. Writes the appropriate message about
#' MDL in the readme.
#'
#' @param qaqc_dir file path to the mdl files generated with \link[eemanalyzeR]{create_mdl}
#' @param type Either "mdl" or "check-std" to specify the type of QA/QC files to return.
#' @param quiet Logical. Should function warn if default is used?
#' @param method Character of the method to use for the QAQC files.
#'
#' @export
#' @examples
#' #No directory will return NULL
#' get_qaqc(NA, type="mdl", quiet =TRUE)
#'
#' #Otherwise will try to return the requested QAQC files
#' mdl <- get_qaqc(file.path(system.file("extdata", package = "eemanalyzeR")), type = "mdl")
#' plot(mdl$eem_mdl)
get_qaqc <- function(qaqc_dir, type, method=NULL, quiet = FALSE){
  stopifnot(type %in% c("mdl", "check-std"), is.character(method) | is.null(method))

  readme_txt <- ifelse(type == "mdl", "method detection limits (MDL)", "long-term standards")

  #step 1: if qaqc_dir is set to NA, we don't check for qaqc files
  if(is.na(qaqc_dir) || !dir.exists(qaqc_dir) || length(list.files(qaqc_dir)) == 0){
    eem_data <- NULL
    if(!quiet){warning(paste0("Fluorescence ", readme_txt, " is missing and will not be used for checks"))}
    .write_readme_line(paste0("Fluorescence indices were not checked against ", readme_txt), "mdl")

    abs_data <- NULL
    if(!quiet){warning(paste0("Absorbance ", readme_txt, " is missing and will not be used for checks"))}
    .write_readme_line(paste0("Absorbance indices were not checked against ", readme_txt, "\n"), "mdl", append = TRUE)

    method <- NULL
  }else{
    #step 2: see how many files are in QAQC dir (of the right type)
    qaqc_files <- list.files(qaqc_dir, recursive = TRUE, pattern = type)

    eem_files <- grep(paste0("eem-", type, ".rds$"), qaqc_files, value=TRUE, ignore.case = TRUE)
    abs_files <- grep(paste0("abs-", type, ".rds$"), qaqc_files, value=TRUE, ignore.case = TRUE)

    #if method specified use that
    if(is.character(method)){
      eem <- eem_files[gsub(paste0("-eem-", type, ".rds"), "", basename(eem_files), ignore.case = TRUE) == method]
      abs <- abs_files[gsub(paste0("-abs-", type, ".rds"), "", basename(abs_files), ignore.case = TRUE) == method]
    }

    #if more than one detected ask, if non interactive use default with warning
    if(length(eem_files) > 1 | length(abs_files) > 1 & is.null(method)){
      if(rlang::is_interactive()){
        methods <- unique(dirname(c(eem_files, abs_files)))
        cat(paste0("Multiple ", type, " files found:\n",
                   paste(paste0(1:length(methods), ": ", methods), collapse = "\n")))
        keep <- readline("Specify the number of the method to use: ")

        method <- methods[as.numeric(keep)]

      }else{
        if(!quiet){warning("Running non-interactively; default QAQC method files were used.")}
        method <- "default"
      }
      eem <- eem_files[gsub(paste0("-eem-", type, ".rds"), "", basename(eem_files), ignore.case = TRUE) == method]
      abs <- abs_files[gsub(paste0("-abs-", type, ".rds"), "", basename(abs_files), ignore.case = TRUE) == method]
    }else{
      eem <- eem_files
      abs <- abs_files
    }

    #get files and write readme
    eem_data <- readRDS(file.path(qaqc_dir, eem))
    .write_readme_line(paste0("Fluorescence indices were checked against ", readme_txt, " using method ", dirname(eem)), gsub("-", "_", type), append=TRUE)

    abs_data <- readRDS(file.path(qaqc_dir, abs))
    .write_readme_line(paste0("Absorbance indices were checked against ", readme_txt, " using method ", dirname(abs), "\n"), gsub("-", "_", type), append=TRUE)

  }


  #return qaqc files
    files <- list(eem_data, abs_data, method)
    names(files) <- c(paste0(c("eem_", "abs_"), gsub("-", "_", type)), "method")
    return(files)
    }
