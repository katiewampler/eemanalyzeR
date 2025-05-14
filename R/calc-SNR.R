# Calculating Raman Noise for EEM

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
#' @param eem EEM object to have signal to noise ratio calculated - must be a blank!
#'
#' @return numeric signal to noise ratio
#'
calc_raman_SNR_sqrt <- function(eem) {

  # Check that the eem is a blank eem
  stopifnot(.is_eem(eem),
            .is_blank(eem),
            !attr(eem, "is_blank_corrected") # We don't want the blank to have been subtracted
            )

  # Create matrix from EEM
    mat <- get_sample_info(eem, "x")

  # Calculate the signal at specific points
  S_397nm <- mat[350, 397]
  S_450nm <- mat[350, 450]
  # Calc Signal-to-noise ratio
  SNR <- ( S_397nm - S_450nm ) / sqrt(S_450nm)

  return(SNR)
}
