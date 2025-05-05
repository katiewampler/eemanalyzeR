#' Calculate the Water Raman SNR ratio using Horiba's Square Root Method
#'
#' The peak signal is measured at the water Raman peak intensity at 397 nm (for 350 nm
#' excitation) and the noise in a region where no Raman signal is present (450 nm).
#' For a perfect optical system there would be no signal at 450 nm since there is
#' no Raman emission there, however, all electro-optical systems have some levels
#' of stray light and noise, which will contribute to a signal at 450 nm. The above
#' formula assumes that the noise is governed by Poisson statistics and, therefore,
#' can be calculated as the square root of the baseline signal counts at 450 nm.
#' It is only applicable to photon counting detection, so for comparison purposes
#' it should only be used when comparing two photon counting spectrofluorometers.
#'
#' @param eem an \code{eem} object to have signal to noise ratio calculated. See details for more info.
#'
#' @return numeric signal to noise ratio
#'
#' @details
#' Calculation must be done on a blank, if \code{eem} is a blank it will calculate on the sample \code{eem$x}.
#' If \code{eem} is not a blank, the associated instrument blank will be used, which means that the blank must be
#' added using the function \link[eemanalyzeR]{add_blanks}.
#'
#' @export
#' @source Horiba (TODO)
#'
#' @examples
#' eemlist <- add_metadata(metadata, example_eems)
#' eemlist <- add_blanks(eemlist, validate=FALSE)
#' SNR <- calc_raman_SNR_sqrt(eemlist[[1]])
#'
calc_raman_SNR_sqrt <- function(eem) {
  # Check that the eem is a blank eem
  stopifnot(.is_eem(eem),.blk_added(eem) | .is_blank(eem))

  # Calculate the signal at specific points
  if(.is_blank(eem)){
    S397 <- as.numeric(get_fluorescence(eem, ex=350, em=397))
    S450 <- as.numeric(get_fluorescence(eem, ex=350, em=450))
  }else{
    #to allow using get_fluorescence, make "blank" where x is replaced with blk_x
    eem_blk <- eem
    eem_blk$x <- get_sample_info(eem, "blk_x")

    S397 <- as.numeric(get_fluorescence(eem_blk, ex=350, em=397))
    S450 <- as.numeric(get_fluorescence(eem_blk, ex=350, em=450))
  }


  # Calc Signal-to-noise ratio
  SNR <- ( S397 - S450 ) / sqrt(S450)

  return(SNR)

}
