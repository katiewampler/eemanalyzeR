#' Down-scaled excitation emission matrices
#'
#' A subset of water samples run on a Horiba Aqualog for excitation emission matrices (EEMs). This data includes
#' both the samples and blanks. Data was loaded using \link[eemanalyzeR]{eem_dir_read}.
#' For more details on the format of an eemlist, see the \link[eemR]{eemR} package.
#'
#' @format An \code{eemlist} with six samples:
#' \itemize{
#'   \item{\strong{B1S1ExampleBlankBEM}: Fluorescence measured for a blank of Type 1 water}
#'   \item{\strong{B1S1ExampleBlankSEM}: Fluorescence measured for a sample of Type 1 water}
#'   \item{\strong{B1S2ExampleTeaStdBEM}: Fluorescence measured for a blank of Type 1 water}
#'   \item{\strong{B1S2ExampleTeaStdSEM}: Fluorescence measured for a sample of dilute unsweetened tea}
#'   \item{\strong{B1S3ExampleSampleBEM}: Fluorescence measured for a blank of Type 1 water}
#'   \item{\strong{B1S3ExampleSampleSEM}: Fluorescence measured for a sample of stream water from Oregon, USA}
#' }
#' @source Oregon State University Forest Ecohydrology and Watershed Sciences Lab (2022-11-14)
"example_eems"

#' Down-scaled absorbance data
#'
#' A subset of water samples run on a Horiba Aqualog for absorbance. This data only includes
#' the absorbance data associated with the samples in \link{example_eems}. Data was loaded using the
#' \link[eemanalyzeR]{abs_dir_read} function. For more details on the format of an eemlist, see \link[eemanalyzeR]{abs_read}.

#'
#' @format An \code{abslist} with three samples
#' \itemize{
#'   \item{\strong{B1S1ExampleBlankABS}: Absorbance measured for a sample of Type 1 water}
#'   \item{\strong{B1S2ExampleTeaStdABS}: Absorbance measured for a sample of dilute unsweetened tea}
#'   \item{\strong{B1S3ExampleSampleABS}: Absorbance measured for a sample of stream water from Oregon, USA}
#' }
#' @source Oregon State University Forest Ecohydrology and Watershed Sciences Lab (2022-11-14)
"example_abs"

#' Down-scaled and processed excitation emission matrices
#'
#' \link[eemanalyzeR]{example_eems} processed using \code{eemanalyzeR}.
#'
#' @format An \code{eemlist} with three samples:
#' \itemize{
#'   \item{\strong{B1S1ExampleBlankSEM}: Fluorescence measured for a sample of Type 1 water}
#'   \item{\strong{B1S2ExampleTeaStdSEM}: Fluorescence measured for a sample of dilute unsweetened tea}
#'   \item{\strong{B1S3ExampleSampleSEM}: Fluorescence measured for a sample of stream water from Oregon, USA}
#' }
#' @source Oregon State University Forest Ecohydrology and Watershed Sciences Lab (2022-11-14)
"example_processed_eems"

#' Down-scaled and processed absorbance data
#'
#' \link[eemanalyzeR]{example_abs} processed using \code{eemanalyzeR}.
#'
#' @format An \code{abslist} with three samples
#' \itemize{
#'   \item{\strong{B1S1ExampleBlankABS}: Absorbance measured for a sample of Type 1 water}
#'   \item{\strong{B1S2ExampleTeaStdABS}: Absorbance measured for a sample of dilute unsweetened tea}
#'   \item{\strong{B1S3ExampleSampleABS}: Absorbance measured for a sample of stream water from Oregon, USA}
#' }
#' @source Oregon State University Forest Ecohydrology and Watershed Sciences Lab (2022-11-14)
"example_processed_abs"

#' Example metadata
#'
#' Metadata associated with the samples in \link{example_eems}. Provides an example of the structure
#' and requirements for metadata needed to run samples.
#'
#' @format A data.frame with 3 rows and 11 columns:
#' \describe{
#'   \item{index}{Simply the numeric order of entry (e.g. 1, 2, 3, 4, etc...)}
#'   \item{analysis_date}{optional: Date samples were ran on instrument (not collected in the field)}
#'   \item{description}{optional: A brief description of the sample collected}
#'   \item{data_identifier}{The file name from the aqualog, needs to match exactly}
#'   \item{replicate_no}{A number indicating if the sample was replicated, an unreplicated sample should get a 1. Analytical replicates must have identical sample names.}
#'   \item{data_identifier}{The file name from the aqualog, needs to match exactly}
#'   \item{integration_time_s}{Integration time of sample (e.g. 1, 2, etc...)}
#'   \item{dilution}{Include dilution factor here as decimal format (e.g. a 2-fold dilution with 1 part sample and 1 part water will have a dilution factor of 0.5). If no dilution, put 1}
#'   \item{RSU_area_1s}{The RSU Adjust Area from the Raman Test, used to normalize the data from day to day}
#'   \item{run_type}{How the samples were run (manual or sampleQ)}
#'   \item{collect_date}{optional: The date the water samples were collected}
#'   \item{DOC_mg_L}{optional: DOC concentration in mg/L of the original sample (not the diluted sample)}
#'   \item{Notes}{optional: Any notes from sample collection or sample running, will not be used in analysis}
#' }
#' @source Oregon State University Forest Ecohydrology and Watershed Sciences Lab (2022-11-14)
"metadata"


#' Long Term Average Blank
#'
#' An \code{eem} object containing the average of 81 blank samples, used to check for blank consistency.
#'
#' @format an \code{eem} object, for more details on the format of an eem, see the \link[eemR]{eemR} package.
#' @note for details on creating your own long-term blank see ##to do: make function or vignette here
#' @source Oregon State University Forest Ecohydrology and Watershed Sciences Lab (2022 to 2024)
"longterm_blank"


#' Tea Absorbance Model
#'
#' An tibble containing the average absorbance of multiple 1% Pure Leaf Unsweetened Black Tea (SRMtea) standards.
#'
#' @format tibble with columns: wavelength, mean_abs_by_wavelength, sd_abs_by_wavelength, sdmin_3x, sdmax_3x
#' @note for details on creating your own long-term SRMtea see ##to do: make function or vignette here
#' @source Oregon State University Forest Ecohydrology and Watershed Sciences Lab (2022 to 2024)
"tea_absorbance_model"

#' Standard Ranges for Absorbance and EEMs Indices
#'
#' A data.frame containing the minimum and maximum range expected for index values generated using
#' \link[eemanalyzeR]{get_indices}. Used to flag indices that warrant further examination to ensure they
#' are accurate.
#'
#'@format A data.frame with 62 rows and 5 columns:
#' \describe{
#'   \item{index_method}{the index method used to calculate the index}
#'   \item{index}{the name of the index metric calculated}
#'   \item{low_val}{the minimum value expected for the index based on literature sources}
#'   \item{high_val}{the maximum value expected for the index based on literature sources}
#'   \item{source}{The sources used to determine the minimum and maximum values}
#' }
#'
#' @source
#' Fischer, S. J., Fegel, T. S., Wilkerson, P. J., Rivera, L., Rhoades, C. C., & Rosario-Ortiz, F. L. (2023). Fluorescence and Absorbance Indices for Dissolved Organic Matter from Wildfire Ash and Burned Watersheds. ACS ES&T Water, 3(8), 2199-2209. \url{https://doi.org/10.1021/acsestwater.3c00017}
#'
#' Galgani, L., Engel, A., Rossi, C., Donati, A., & Loiselle, S. A. (2018). Polystyrene microplastics increase microbial release of marine Chromophoric Dissolved Organic Matter in microcosm experiments. Scientific Reports, 8(1), 14635. \url{https://doi.org/10.1038/s41598-018-32805-4}
#'
#' Hansen, A. M., Kraus, T. E. C., Pellerin, B. A., Fleck, J. A., Downing, B. D., & Bergamaschi, B. A. (2016). Optical properties of dissolved organic matter (DOM): Effects of biological and photolytic degradation. Limnology and Oceanography, 61(3), 1015-1032. \url{https://doi.org/10.1002/lno.10270}
#'
#' Hansen, A. M., Fleck, J., Kraus, T. E. C., Downing, B. D., von Dessonneck, T., & Bergamaschi, B. (2018). Procedures for using the Horiba Scientific Aqualog® fluorometer to measure absorbance and fluorescence from dissolved organic matter (USGS Numbered Series No. 2018-1096). Procedures for using the Horiba Scientific Aqualog® fluorometer to measure absorbance and fluorescence from dissolved organic matter (Vol. 2018-1096). Reston, VA: U.S. Geological Survey. \url{https://doi.org/10.3133/ofr20181096}
#'
#' Helms, J. R., Stubbins, A., Ritchie, J. D., Minor, E. C., Kieber, D. J., & Mopper, K. (2008). Absorption spectral slopes and slope ratios as indicators of molecular weight, source, and photobleaching of chromophoric dissolved organic matter. Limnology and Oceanography, 53(3), 955-969. \url{https://doi.org/10.4319/lo.2008.53.3.0955}
#'
#' Korak, J. A., & McKay, G. (2024). Meta-Analysis of Optical Surrogates for the Characterization of Dissolved Organic Matter. Environmental Science & Technology, 58(17), 7380-7392. \url{https://doi.org/10.1021/acs.est.3c10627}
#'
#' Li, Y., Zhang, Y., Li, Z., Wan, J., Dang, C., & Fu, J. (2022). Characterization of colored dissolved organic matter in the northeastern South China Sea using EEMs-PARAFAC and absorption spectroscopy. Journal of Sea Research, 180, 102159. \url{https://doi.org/10.1016/j.seares.2021.102159}
#'
#' Meingast, K. M., Kane, E. S., Marcarelli, A. M., Wagenbrenner, J. W., & Beltrone, V. G. (2023). Seasonal trends of DOM character in soils and stream change with snowmelt timing. Water Resources Research. 59(3): e2022WR032014. \url{https://doi.org/10.1029/2022WR032014}
#'
#' Peuravuori, J., & Pihlaja, K. (1997). Molecular size distribution and spectroscopic properties of aquatic humic substances. Analytica Chimica Acta, 337(2), 133-149. \url{https://doi.org/10.1016/S0003-2670(96)00412-6}
#'
#' Weishaar, J. L., Aiken, G. R., Bergamaschi, B. A., Fram, M. S., Fujii, R., & Mopper, K. (2003). Evaluation of Specific Ultraviolet Absorbance as an Indicator of the Chemical Composition and Reactivity of Dissolved Organic Carbon. Environmental Science & Technology, 37(20), 4702-4708. \url{https://doi.org/10.1021/es030360x}
#'
#' Zalba, P., Amiotti ,Nilda M., Galantini ,Juan A., & and Pistola, S. (2016). Soil Humic and Fulvic Acids from Different Land-Use Systems Evaluated By E4/E6 Ratios. Communications in Soil Science and Plant Analysis, 47(13-14), 1675-1679. \url{https://doi.org/10.1080/00103624.2016.1206558}
#'
#' Zsolnay, A., Baigar, E., Jimenez, M., Steinweg, B., & Saccomandi, F. (1999). Differentiating with fluorescence spectroscopy the sources of dissolved organic matter in soils subjected to drying. Chemosphere, 38(1), 45-50. \url{https://doi.org/10.1016/S0045-6535(98)00166-0}

"indice_ranges"
