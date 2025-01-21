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
#' @source Oregon State University Forest Watershed Hydrology Lab (2022-11-14)
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
#' @source Oregon State University Forest Watershed Hydrology Lab (2022-11-14)
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
#' @source Oregon State University Forest Watershed Hydrology Lab (2022-11-14)
"example_absorbance"

#' Example metadata
#'
#' Metadata associated with the samples in \link{example_samples}. Provides an example of the structure
#' and requirements for metadata needed to run samples.
#'
#' @format A data.frame with 3 rows and 11 columns:
#' \describe{
#'   \item{index}{Simply the numeric order of entry (e.g. 1, 2, 3, 4, etc…)}
#'   \item{analysis_date}{Date samples were ran on instrument (not collected in the field)}
#'   \item{description}{A brief description of the sample collected}
#'   \item{data_identifier}{The file name from the aqualog, needs to match exactly}
#'   \item{replicate_no}{A number indicating if the sample was replicated, an unreplicated sample should get a 1. Analytical replicates must have identical sample names.}
#'   \item{data_identifier}{The file name from the aqualog, needs to match exactly}
#'   \item{integration_time_s}{Integration time of sample (e.g. 1, 2, etc…)}
#'   \item{dilution}{Include dilution factor here as decimal format (e.g. a 2-fold dilution with 1 part sample and 1 part water will have a dilution factor of 0.5). If no dilution, put 1}
#'   \item{RSU_area_1s}{The RSU Adjust Area from the Raman Test, used to normalize the data from day to day}
#'   \item{run_type}{How the samples were run (manual or sampleQ)}
#'   \item{collect_date}{optional: The date the water samples were collected}
#'   \item{DOC_mg_L}{optional: DOC concentration in mg/L of the original sample (not the diluted sample)}
#'   \item{Notes}{optional: Any notes from sample collection or sample running, will not be used in analysis}
#' }
#' @source Oregon State University Forest Watershed Hydrology Lab (2022-11-14)
"metadata"
