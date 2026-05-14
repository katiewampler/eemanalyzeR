#' Asks to update QAQC directory in config
#'
#' If `qaqc_dir` is `NA` in the user config file, the code will not check for QAQC tests. However, we want to
#' automatically set the code to use the created QAQC standards. This will ask to update the user_config file, if `Y`,
#' it will write the default storage location for the standards.
#'
#' @returns Result of user input as `TRUE` or `FALSE`.
#'
#' @noRd
update_qaqc_dir <- function(){
  #get paths
  user_dir <- .user_data_dir()
  user_config_file <- .user_config_path()
  new_dir <- file.path(user_dir, "qaqc-stds")

  #see what is in file
  old_qaqc_dir <- read_user_config(user_config_file)$qaqc_dir

  #only ask if it would change what's there
  if(!file.exists(user_config_file) || is.na(old_qaqc_dir) || old_qaqc_dir != new_dir){
    if(!rlang::is_interactive()) {
      update_path <- TRUE
    } else {
      update_path <- .yesorno("Update user config file with QAQC file path?",
                              paste0("qaqc_dir in user config has been updated to ", normalizePath(file.path(rappdirs::user_data_dir(appname = "eemanalyzeR"), "qaqc-stds"))),
                              "Warning: qaqc_dir must be manually specified using `qaqc_dir` to use QAQC files in processing.")
    }

    if(update_path){
      if (!dir.exists(new_dir)) dir.create(new_dir, recursive = TRUE, showWarnings = FALSE)
      # Fix the slashes in the new directory
      new_dir <- normalizePath(new_dir, winslash = "/")
      # Edit the qaqc_directory in the user config and re-load
      edit_user_config(qaqc_dir = new_dir, interactive = FALSE)
      load_user_config()
    }

  }

  qaqc_dir <- get_qaqc_dir()

  invisible(qaqc_dir)
}

#' Look for and load QAQC files
#'
#' Looks for the specified QAQC files (MDL and check standards) within the `qaqc_dir` specified in
#' the config file. If more than one set is found, it will prompt the user to specify which
#' method they want to use. This will be stored in the local copy of the config which will
#' be maintained for the session. Will also provide warnings and write to the `readme` to note if
#' no files were found or what method was used.
#'
#' @param qaqc_dir file path to the mdl files generated with \link[eemanalyzeR]{create_mdl}
#' @param type Either "mdl" or "check-std" to specify the type of QA/QC files to return.
#' @param quiet Logical. Should function warn if default is used?
#' @param method Character of the method to use for the QAQC files.
#'
#' @export
#' @md
#' @returns A list of length 3:
#' 1. eem-*: `NULL` if not found, otherwise the requested QAQC file for eems.
#' 2. abs-*: `NULL` if not found, otherwise the requested QAQC file for absorbance.
#' 3. method: The name of the method chosen, `NULL` if no files are found.
#'
#' @examples
#' #No directory will return NULL
#' get_qaqc(NA, type="mdl", quiet =TRUE)
#'
#' #Otherwise will try to return the requested QAQC files
#' mdl <- get_qaqc(file.path(system.file("extdata", package = "eemanalyzeR")), type = "mdl")
#' plot(mdl$eem_mdl)
get_qaqc <- function(qaqc_dir, type, method = get_qaqc_method(), quiet = TRUE){
  stopifnot(type %in% c("mdl", "check-std"), is.character(method))

  readme_txt <- ifelse(type == "mdl", "method detection limits (MDL)", "long-term standards")

  #step 1: if qaqc_dir is set to NA, we don't check for qaqc files
  if(is.na(qaqc_dir) || !dir.exists(qaqc_dir) || length(list.files(qaqc_dir)) == 0){
    eem_data <- NULL
    if(!quiet){warning(paste0("Fluorescence ", readme_txt, " is missing and will not be used for checks"))}
    .write_readme_line(paste0("Fluorescence indices were not checked against ", readme_txt), gsub("-", "_", type))

    abs_data <- NULL
    if(!quiet){warning(paste0("Absorbance ", readme_txt, " is missing and will not be used for checks"))}
    .write_readme_line(paste0("Absorbance indices were not checked against ", readme_txt, "\n"), gsub("-", "_", type), append = TRUE)

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
    if(length(eem_files) > 1 | length(abs_files) > 1){
      if(rlang::is_interactive() & is.na(method)){
        methods <- unique(dirname(c(eem_files, abs_files)))
        cat(paste0("Multiple ", "QAQC", " files found:\n",
                   paste(paste0(1:length(methods), ": ", methods), collapse = "\n")))
        keep <- readline("Specify the number of the method to use: ")

        method <- methods[as.numeric(keep)]

        modify_session_config(qaqc_method = method)

      }else if(!rlang::is_interactive()){
        warning("Running non-interactively; default QAQC method files were used.")
        method <- "default"
      }
      eem <- eem_files[gsub(paste0("-eem-", type, ".rds"), "", basename(eem_files), ignore.case = TRUE) == method]
      abs <- abs_files[gsub(paste0("-abs-", type, ".rds"), "", basename(abs_files), ignore.case = TRUE) == method]
    }else{
      eem <- eem_files
      abs <- abs_files
    }

    #get files and write readme (only write readme once)
    if(is.null(get_readme()) || is.na(get_readme()$mdl)){
      .write_readme_line(paste0("Fluorescence indices were checked against ", readme_txt, " using method ", dirname(eem)), gsub("-", "_", type))
      .write_readme_line(paste0("Absorbance indices were checked against ", readme_txt, " using method ", dirname(abs), "\n"), gsub("-", "_", type), append=TRUE)
    }

    eem_data <- readRDS(file.path(qaqc_dir, eem))
    abs_data <- readRDS(file.path(qaqc_dir, abs))


  }


  #return qaqc files
  files <- list(eem_data, abs_data, method)
  names(files) <- c(paste0(c("eem_", "abs_"), gsub("-", "_", type)), "method")
  return(files)
}

#' Check for differing wavelengths between sample and QAQC file
#'
#' @param qaqc_dir file path to the mdl files generated with \link[eemanalyzeR]{create_mdl}
#' @param type Either "mdl" or "check-std" to specify the type of QA/QC files to return.
#' @param eemlist An `eemlist` object.
#' @param abslist An `abslist` object.
#'
#' @returns Silent `NULL`
#' @noRd
#'
check_qaqc_wave <- function(eemlist, abslist, qaqc_dir = get_qaqc_dir(), type){
  #get mdls
  qaqc <- get_qaqc(qaqc_dir, type, quiet =FALSE)

  #text for warning
  warning_txt <- ifelse(type == "mdl", "method detection limits (MDL)", "long-term standards")
  warning_fun <- ifelse(type == "mdl", "check-mdl", "check-std")

  #guard against no qaqc files
  if(any(sapply(qaqc, is.null))){
    return(invisible(NULL))
  }

  #check eemlist
  std_em <- get_sample_info(qaqc[[1]], "em")
  samp_em <- unique(as.numeric(get_sample_info(eemlist, "em")))
  std_ex <- get_sample_info(qaqc[[1]], "ex")
  samp_ex <- unique(as.numeric(get_sample_info(eemlist, "ex")))
  if(length(base::setdiff(samp_em, std_em)) > 0 | length(base::setdiff(samp_ex, std_ex)) > 0){
    diff_eem <- TRUE
  }else{diff_eem <- FALSE}

  # check abslist
  std_wave <- get_sample_info(qaqc[[2]], "data")[,1]
  samp_wave <- get_sample_info(abslist, "data")[,1]
  if(length(base::setdiff(samp_wave, std_wave)) > 0){
    diff_abs <- TRUE
  }else{diff_abs <- FALSE}


  #provide warning
  if(any(c(diff_eem, diff_abs))){
    warning(paste0("Wavelengths differ between ", warning_txt, " and data, `", warning_fun, "` may be unreliable."))
  }

  return(invisible(NULL))
}
