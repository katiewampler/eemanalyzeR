#' Safely return ratios values
#'
#' Computes a ratio and returns either the calculated value or a QA/QC flag
#' if the ratio cannot be computed.
#'
#' @details
#' The following conditions are checked before returning a value:
#'
#' - **DATA_01**: Missing data required to calculate the index
#' - **DATA_03**: Denominator is zero, so the ratio cannot be calculated
#'
#' @param val1 Numerator value(s); may be a single value or a vector.
#' @param val2 Denominator value(s); may be a single value or a vector.
#'
#' @return A numeric ratio where possible, otherwise a character QA/QC flag.
#'
#' @examples
#' # Calculate the ratio of Peak A to Peak T
#' pA <- get_fluorescence(example_eems, ex = 250:260, em = 380:480)
#' pT <- get_fluorescence(example_eems, ex = 270:280, em = 320:350)
#' rAT <- get_ratios(pA, pT)
#'
#' @export
#' @md
get_ratios <- function(val1, val2) {
  stopifnot(length(val1) == length(val2) | length(val1) == 1 | length(val2) == 1)

  # if only one val given, make length of other
  if (length(val1) == 1) {
    val1 <- rep(val1, length(val2))
  }
  if (length(val2) == 1) {
    val2 <- rep(val2, length(val1))
  }

  # ensure numeric
  # remove flags if needed
  val1 <- stringr::str_split_i(val1, "_", i = 1)
  val2 <- stringr::str_split_i(val2, "_", i = 1)

  # if flag is a DATA01, will get "DATA" replace with NA
  val1[grepl("DATA|NOISE", val1)] <- NA
  val2[grepl("DATA|NOISE", val2)] <- NA

  val1 <- as.numeric(val1)
  val2 <- as.numeric(val2)

  vals <- c()
  for (x in 1:length(val1)) {
    if (is.na(val1[x]) | is.na(val2[x])) {
      val <- "DATA01"
    } else if (val2[x] == 0) {
      val <- "DATA03"
    } else {
      val <- unname(val1[x] / val2[x])
    }
    vals <- c(vals, val)
  }

  # add replace DATA flag with noise flag where needed
  vals[val1 == "NOISE01" | val2 == "NOISE01"] <- "NOISE01"

  return(vals)
}
