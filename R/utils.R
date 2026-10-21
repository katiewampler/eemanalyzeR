
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
#' @examples
#' eem <- example_eems[1] #can get a single eem (still as eemlist)
#'
#' eems <- example_eems[1:3] #or get several
#'
`[.eemlist` <- function(eemlist, i) {
  sublist <- NextMethod()
  structure(sublist, class = "eemlist")
}

# Overload the bracket operator for eemlist subsetting
#' Subsetting using `[[` for eemlist - we want this to return an eem
#'
#' @param eemlist the eemlist to subset
#' @param i the index for subsetting
#'
#' @export
#' @keywords internal
#' @md
#' @returns an object of class `eem`
#' @method [[ eemlist
#' @examples
#' eem <- example_eems[[1]]
#'
`[[.eemlist` <- function(eemlist, i) {
  sublist <- NextMethod()
  structure(sublist, class = "eem")
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
#' @examples
#' abs <- example_abs[1] #can get a single absorbance (still as abslist)
#'
#' abs_multiple <- example_abs[1:3] #or get several
#'
`[.abslist` <- function(abslist, i) {
  sublist <- NextMethod()
  structure(sublist, class = "abslist")
}

# Overload the bracket operator for abslist subsetting
# we want to always return an abs

#'Subsetting using `[[` for an abslist
#'
#' @param abslist the abslist to subset
#' @param i the index for subsetting
#'
#' @export
#' @keywords internal
#' @md
#' @returns an object of class `abs`
#' @method [[ abslist
#' @examples
#' abs <- example_abs[[1]]
#'
`[[.abslist` <- function(abslist, i) {
  sublist <- NextMethod()
  structure(sublist, class = "abs")
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


#' Just a nicer way to get the directory where the user config and QAQC files should live
#' @noRd
.user_data_dir <- function(){
  return(file.path(fs::path_norm(rappdirs::user_data_dir(appname = "eemanalyzeR"))))
}

