#' Interpolate absorbance data
#'
#' Interpolates absorbance data to 1 nm resolution so indices can be calculated
#' at wavelengths that were not explicitly measured.
#'
#' @param abs An object of class `abs` or `abslist`.
#' @param type The interpolation method, either "linear" for linear interpolation
#'   using [zoo::na.approx()], or "spline" for spline interpolation using
#'   [zoo::na.spline()].
#'
#' @return An object of class `abs` or `abslist`.
#' @export
#' @md
#' @examples
#' abslist_filled <- abs_interp(example_abs)
#' abs_filled <- abs_interp(example_abs[[1]])
abs_interp <- function(abs, type = "linear") {
  stopifnot(.is_abs(abs) | .is_abslist(abs), type %in% c("linear", "spline"))

  # loop through if abslist
  if (.is_abslist(abs)) {
    res <- lapply(abs, abs_interp)
    class(res) <- "abslist"
    return(res)
  }

  # extract data, and get full range of wavelengths
  abs_val <- data.frame(abs$data)
  res <- merge(abs_val, data.frame(X1 = min(abs_val$X1):max(abs_val$X1)), all = T)

  # interpolate
  if (type == "linear") {
    res <- zoo::na.approx(res)
  } else if (type == "spline") {
    res <- zoo::na.spline(res)
  }

  # reformat
  abs$data <- as.matrix(res)
  abs$n <- nrow(res)
  return(abs)
}
