
#' Read a single absorbance file into R
#'
#' Will accept absorbance files run via Aqualog exported using the sample Q or manually. If the function tries
#' to load an EEM's file it will return a warning and a NULL value for absorbance.
#'
#' @param file file path to the absorbance file to load
#' @importFrom stringr str_extract_all
#' @importFrom tools file_ext
#'
#' @return An object of class \code{abs} containing:
#' \itemize{
#'  \item file The filename of the absorbance data.
#'  \item sample The sample name of the absorbance data.
#'  \item n The number of wavelengths absorbance was measured at.
#'  \item data A matrix where the first column is the wavelengths in nm and the second column is the absorbance.
#'  \item location Directory of the absorbance data.
#' }
#' @export
#'
#' @examples
#' abs_files <- list.files(system.file("extdata", package = "eemanalyzeR"),
#' full.names=TRUE, pattern="ABS")
#' abs <- abs_read(abs_files[1])

abs_read <- function(file){
    #captures warning from trying to rbind eem's data

    .safe_rbind <- function(abs){
      tryCatch({abs <- do.call(rbind, abs)
      return(abs)},
      warning = function(w) {
        # Check if it's a specific error
        if (grepl("number of columns of result is not a multiple of vector length", conditionMessage(w))) {
          warning("Unable to import file: ", file, ".\nPlease use the 'pattern' and 'skip' arguments to ensure only absorbance files are selected.", call. = FALSE)
          return(NULL)
        } else {
          stop("An unexpected error occurred: ", conditionMessage(w), call. = FALSE)}})
    }

    data <- readLines(file)
    abs <- stringr::str_extract_all(data, "-?\\d+(?:\\.\\d*)?(?:[eE][+\\-]?\\d+)?")
    abs <- lapply(abs, as.numeric)

    #if abs files was exported manually, remove header rows and extra columns in row 1
    if(length(abs[[3]]) == 0){
      #top row has two extra items
      abs[[4]] <- abs[[4]][-c(3,5)]

      #remove header items
      abs <- abs[-c(1:3)]

      #merge into df
      abs <- do.call(rbind, abs)
      abs <- abs[,c(1,8)]
    }else{
      abs <- .safe_rbind(abs)
    }


    #give column names and make into df if not skipped
    if(is.null(abs) == F){
      #thrown an error if the wavelength isn't continuous, suggesting transmittance data was added
      if(sum(diff(abs[1,]) > 0) > 0){
        stop("wavelengths aren't continuous, please ensure transmittance data wasn't included in absorbance file:\n", file)
      }

      #if there's an NA value, it'll get the value of the wavelength, replace with NA
      abs[abs[,1] == abs[,2],2] <- NA

      #create into class "abs"
      obj <- list(file = file,
                  sample = gsub(paste0("[.]", file_ext(file)), "",basename(file)),
                  n = nrow(abs),
                  data = abs,
                  location =dirname(file)
      )

      class(obj) <- "abs"

      attr(obj, "is_dil_corrected") <- FALSE
      attr(obj, "is_doc_normalized") <- FALSE
      # Default to none and add them later
      attr(obj, "sample_type") <- "none"

    }else{
      obj <- NULL
    }



    return(obj)
  }


