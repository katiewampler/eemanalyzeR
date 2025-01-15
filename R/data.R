#' Down-scaled excitation emission matrices of sample blanks
#'
#' A subset of water samples run on a Horiba Aqualog for excitation emission matrices (EEMs). This data only includes
#' the blanks associated with the samples in \link{example_samples}. Data was loaded using \link[eemR]{eem_read}.
#' For more details on the format of an eemlist, see the \link[eemR]{eemR} package.
#'
#' @format  An eemlist with three samples:
#' \describe{
#'   \item{ExampleBlank}{Fluorescence measured for a blank of Type 1 water}
#'   \item{ExampleTeaStd}{Fluorescence measured for a blank of Type 1 water}
#'   \item{ExampleSample}{Fluorescence measured for a blank of Type 1 water}
#' }
#' @source Oregon State University Forest Watershed Hydrology Lab
"example_blanks"

#' Down-scaled excitation emission matrices of samples
#'
#' A subset of water samples run on a Horiba Aqualog for excitation emission matrices (EEMs). This data only includes
#' the samples associated with the blanks in \link{example_blanks}. Data was loaded using \link[eemR]{eem_read}.
#' For more details on the format of an eemlist, see the \link[eemR]{eemR} package.
#'
#' @format An eemlist with three samples:
#' \describe{
#'   \item{ExampleBlank}{Fluorescence measured for a sample of Type 1 water}
#'   \item{ExampleTeaStd}{Fluorescence measured for a sample of dilute unsweetened tea}
#'   \item{ExampleSample}{Fluorescence measures for a sample of stream water from Oregon, USA}
#' }
#' @source Oregon State University Forest Watershed Hydrology Lab
"example_samples"

#' Down-scaled absorbance data
#'
#' A subset of water samples run on a Horiba Aqualog for absorbance. This data only includes
#' the absorbance data associated with the samples in \link{example_samples}.
#'
#' @format A data.frame with 32 rows and 4 columns:
#' \describe{
#'   \item{wavelength}{Wavelength absorbance was measured at in nanometers}
#'   \item{ExampleBlank}{Absorbance measured for a sample of Type 1 water}
#'   \item{ExampleTeaStd}{Absorbance measured for a sample of dilute unsweetened tea}
#'   \item{ExampleSample}{Absorbance measures for a sample of stream water from Oregon, USA}
#' }
#' @source Oregon State University Forest Watershed Hydrology Lab
"example_absorbance"
