
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


