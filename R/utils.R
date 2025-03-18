
#Functions to check if the objects are the right class (eem/eemlist)

  #' Checks if object is an eem
  #'
  #' @param eem an object
  #' @noRd
  #' @source This function was directly pulled from \link[staRdom]
  .is_eem <- function(eem) {
    ifelse(class(eem) == "eem", TRUE, FALSE)
  }

  #' Checks if object is an eemlist
  #'
  #' @param eem an object
  #' @noRd
  #' @source This function was directly pulled from \link[staRdom]
  .is_eemlist <- function(eem) {
    ifelse(class(eem) == "eemlist", TRUE, FALSE)
  }

  #' Checks if sample is a blank
  #'
  #' @param obj an object
  #' @noRd
  .is_blank <- function(obj) {
    # Get the attribute
    val <- attr(obj, "is_blank")
    stopifnot(is.logical(val))
    return(val)
  }

  #' Checks if sample is a check (e.g. tea standard)
  #'
  #' @param obj an object
  #' @noRd
  .is_check <- function(obj) {
    # Get the attribute
    val <- attr(obj, "is_check")
    stopifnot(is.logical(val))
    return(val)
  }


  #abs_list <- function()
