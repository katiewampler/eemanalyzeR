#' Down-scaled excitation emission matrices
#'
#' A subset of water samples run on a Horiba Aqualog for excitation emission matrices (EEMs). This data includes
#' both the samples and blanks. Data was loaded using [eem_dir_read()].
#' For more details on the format of an eemlist, see the [eemR] package.
#'
#' @md
#' @format An `eemlist` with eight samples:
#' - **B1S1ExampleBlankBEM**: Fluorescence measured for a blank of Type 1 water
#' - **B1S1ExampleBlankSEM**: Fluorescence measured for a sample of Type 1 water
#' - **B1S2ExampleTeaStdBEM**: Fluorescence measured for a blank of Type 1 water
#' - **B1S2ExampleTeaStdSEM**: Fluorescence measured for a sample of dilute unsweetened tea
#' - **B1S3ExampleSampleBEM**: Fluorescence measured for a blank of Type 1 water
#' - **B1S3ExampleSampleSEM**: Fluorescence measured for a sample of stream water from Oregon, USA
#' - **ManualExampleTeaWaterfallPlotBlank**: Fluorescence measured for a blank of Type 1 water
#' - **ManualExampleTeaWaterfallPlotSample**: Fluorescence measured for a sample of dilute unsweetened tea
#'
#' @source Oregon State University Forest Ecohydrology and Watershed Sciences Lab (2022-11-14 and 2022-08-05).
"example_eems"

#' Down-scaled absorbance data
#'
#' A subset of water samples run on a Horiba Aqualog for absorbance. This data only includes
#' the absorbance data associated with the samples in [example_eems()]. Data was loaded using [abs_dir_read()].
#' For more details on the format of an abslist, see [abs_read()].
#'
#' @md
#' @format An `abslist` with four samples:
#' - **B1S1ExampleBlankABS**: Absorbance measured for a sample of Type 1 water
#' - **B1S2ExampleTeaStdABS**: Absorbance measured for a sample of dilute unsweetened tea
#' - **B1S3ExampleSampleABS**: Absorbance measured for a sample of stream water from Oregon, USA
#' - **ManualExampleTeaAbsSpectraGraphs**: Absorbance measured for a sample of dilute unsweetened tea
#'
#' @source Oregon State University Forest Ecohydrology and Watershed Sciences Lab (2022-11-14 and 2022-08-05).
"example_abs"


#' Down-scaled and processed excitation emission matrices
#'
#' Processed version of [example_eems()] using `eemanalyzeR`.
#' @md
#' @format An `eemlist` with four samples:
#' - **B1S1ExampleBlankSEM**: Fluorescence measured for a sample of Type 1 water
#' - **B1S2ExampleTeaStdSEM**: Fluorescence measured for a sample of dilute unsweetened tea
#' - **B1S3ExampleSampleSEM**: Fluorescence measured for a sample of stream water from Oregon, USA
#' - **ManualExampleTeaWaterfallPlotSample**: Fluorescence measured for a sample of dilute unsweetened tea
#'
#' @source Oregon State University Forest Ecohydrology and Watershed Sciences Lab (2022-11-14)
"example_processed_eems"

#' Down-scaled and processed absorbance data
#'
#' Processed version of [example_abs()] using `eemanalyzeR`.
#' @md
#' @format An `abslist` with four samples:
#' - **B1S1ExampleBlankABS**: Absorbance measured for a sample of Type 1 water
#' - **B1S2ExampleTeaStdABS**: Absorbance measured for a sample of dilute unsweetened tea
#' - **B1S3ExampleSampleABS**: Absorbance measured for a sample of stream water from Oregon, USA
#' - **ManualExampleTeaAbsSpectraGraphs**: Absorbance measured for a sample of dilute unsweetened tea
#'
#' @source Oregon State University Forest Ecohydrology and Watershed Sciences Lab (2022-11-14)
"example_processed_abs"

#' Example metadata
#'
#' Metadata associated with the samples in [example_eems()] and [example_abs()]. Provides an example of the structure
#' and requirements for metadata needed to run samples.
#' @md
#' @format A data.frame with 4 rows and 11 columns:
#' - **index**: Numeric order of entry (e.g., 1, 2, 3, etc.)
#' - **analysis_date**: Optional, date samples were run on instrument (not collected in the field)
#' - **description**: Optional, brief description of the sample collected
#' - **data_identifier**: REQUIRED, file name from the aqualog, must match exactly
#' - **replicate_no**: REQUIRED, number indicating if the sample was replicated; unreplicated = 1
#' - **integration_time_s**: REQUIRED, integration time of sample (e.g., 1, 2, etc.)
#' - **dilution**: REQUIRED, dilution factor as decimal (e.g., 0.5 for 2-fold dilution); 1 if no dilution
#' - **RSU_area_1s**: REQUIRED, RSU Adjust Area from the Raman test for normalization
#' - **sample_type**: Optional, flag (sample, sblank, or check)
#' - **run_type**: Optional, how the samples were run (manual or sampleQ)
#' - **collect_date**: Optional, date the water samples were collected
#' - **DOC_mg_L**: Optional, DOC concentration in mg/L of original sample
#' - **Notes**: Optional, any notes from collection or running
#'
#' @source Oregon State University Forest Ecohydrology and Watershed Sciences Lab (2022-11-14)
"metadata"

#' Standard ranges for absorbance and EEMs indices
#'
#' A data.frame containing the minimum and maximum range expected for index values generated using [get_indices()].
#' Used to flag indices that warrant further examination to ensure accuracy.
#' @md
#' @format A data.frame with 62 rows and 5 columns:
#' - **index_method**: The index method used to calculate the index
#' - **index**: Name of the index metric calculated
#' - **low_val**: Minimum value expected based on literature sources
#' - **high_val**: Maximum value expected based on literature sources
#' - **source**: Sources used to determine minimum and maximum values
#'
#' @source
#' Fischer, S. J., Fegel, T. S., Wilkerson, P. J., Rivera, L., Rhoades, C. C., & Rosario-Ortiz, F. L. (2023).
#' Fluorescence and Absorbance Indices for Dissolved Organic Matter from Wildfire Ash and Burned Watersheds. ACS ES&T Water, 3(8),
#' 2199-2209. https://doi.org/10.1021/acsestwater.3c00017
#'
#' Galgani, L., Engel, A., Rossi, C., Donati, A., & Loiselle, S. A. (2018). Polystyrene microplastics increase microbial release of
#' marine Chromophoric Dissolved Organic Matter in microcosm experiments. Scientific Reports, 8(1), 14635. https://doi.org/10.1038/s41598-018-32805-4
#'
#' Hansen, A. M., Kraus, T. E. C., Pellerin, B. A., Fleck, J. A., Downing, B. D., & Bergamaschi, B. A. (2016). Optical properties
#' of dissolved organic matter (DOM): Effects of biological and photolytic degradation. Limnology and Oceanography, 61(3), 1015-1032.
#' https://doi.org/10.1002/lno.10270
#'
#' Hansen, A. M., Fleck, J., Kraus, T. E. C., Downing, B. D., von Dessonneck, T., & Bergamaschi, B. (2018). Procedures for using the Horiba
#' Scientific Aqualog® fluorometer to measure absorbance and fluorescence from dissolved organic matter (USGS Numbered Series No. 2018-1096).
#' Procedures for using the Horiba Scientific Aqualog® fluorometer to measure absorbance and fluorescence from dissolved organic matter (Vol.
#' 2018-1096). Reston, VA: U.S. Geological Survey. https://doi.org/10.3133/ofr20181096
#'
#' Helms, J. R., Stubbins, A., Ritchie, J. D., Minor, E. C., Kieber, D. J., & Mopper, K. (2008). Absorption spectral slopes and slope ratios
#' as indicators of molecular weight, source, and photobleaching of chromophoric dissolved organic matter. Limnology and Oceanography, 53(3),
#' 955-969. https://doi.org/10.4319/lo.2008.53.3.0955
#'
#' Korak, J. A., & McKay, G. (2024). Meta-Analysis of Optical Surrogates for the Characterization of Dissolved Organic Matter. Environmental
#' Science & Technology, 58(17), 7380-7392. https://doi.org/10.1021/acs.est.3c10627
#'
#' Li, Y., Zhang, Y., Li, Z., Wan, J., Dang, C., & Fu, J. (2022). Characterization of colored dissolved organic matter in the northeastern
#' South China Sea using EEMs-PARAFAC and absorption spectroscopy. Journal of Sea Research, 180, 102159. https://doi.org/10.1016/j.seares.2021.102159
#'
#' Meingast, K. M., Kane, E. S., Marcarelli, A. M., Wagenbrenner, J. W., & Beltrone, V. G. (2023). Seasonal trends of DOM character in soils
#' and stream change with snowmelt timing. Water Resources Research. 59(3): e2022WR032014. https://doi.org/10.1029/2022WR032014
#'
#' Peuravuori, J., & Pihlaja, K. (1997). Molecular size distribution and spectroscopic properties of aquatic humic substances. Analytica
#' Chimica Acta, 337(2), 133-149. https://doi.org/10.1016/S0003-2670(96)00412-6
#'
#' Weishaar, J. L., Aiken, G. R., Bergamaschi, B. A., Fram, M. S., Fujii, R., & Mopper, K. (2003). Evaluation of Specific Ultraviolet
#' Absorbance as an Indicator of the Chemical Composition and Reactivity of Dissolved Organic Carbon. Environmental Science & Technology,
#' 37(20), 4702-4708. https://doi.org/10.1021/es030360x
#'
#' Zalba, P., Amiotti ,Nilda M., Galantini ,Juan A., & and Pistola, S. (2016). Soil Humic and Fulvic Acids from Different Land-Use Systems
#' Evaluated By E4/E6 Ratios. Communications in Soil Science and Plant Analysis, 47(13-14), 1675-1679. https://doi.org/10.1080/00103624.2016.1206558
#'
#' Zsolnay, A., Baigar, E., Jimenez, M., Steinweg, B., & Saccomandi, F. (1999). Differentiating with fluorescence spectroscopy the sources of
#' dissolved organic matter in soils subjected to drying. Chemosphere, 38(1), 45-50. https://doi.org/10.1016/S0045-6535(98)00166-0

"indice_ranges"

#' Default configuration of arguments for eemanalyzeR
#'
#' The default argument values used in the [run_eems()] function. Argument
#' values are stored in a package environment (`.pkgenv`) which will be accessed
#' in the case where a specific variable is not specified by the user.
#'
#' @details
#' These values can be edited by:
#' - the user permanently using [edit_user_config()]
#' - the user for the current R session using `modify_config()`
#' - or within the [run_eems()] function itself by providing argument values
#'
#' @md
#' @format A list of length 27:
#' - **abs_pattern**: Used by [abs_dir_read()]. A character string containing a [base::regular expression()]
#'   to match files in `input_dir`. Only files matching the pattern will be loaded.
#' - **abs_skip**: Used by [abs_dir_read()]. A character string containing a [base::regular expression()]
#'   to match files in `input_dir` that should be ignored.
#' - **abs_file_ext**: Used by [abs_dir_read()]. The file extension of the absorbance files.
#' - **abs_recurse_read**: Used by [eem_dir_read()]. Logical. Should the function recursively search directories?
#' - **eem_pattern**: Used by [eem_dir_read()]. A character string containing a [base::regular expression()]
#'   to match files in `input_dir`. Only files matching the pattern will be loaded.
#' - **eem_skip**: Used by [eem_dir_read()]. A character string containing a [base::regular expression()]
#'   to match files in `input_dir` that should be ignored.
#' - **eem_file_ext**: Used by [eem_dir_read()]. The file extension of the EEMs files.
#' - **eem_recurse_read**: Used by [eem_dir_read()]. Logical. Should the function recursively search directories?
#' - **eem_import_func**: Used by [abs_dir_read()]. Character or a user-defined function to import an EEM.
#'   For more details, see [`vignette("custom-indices")`](../doc/custom-indices.html).
#' - **meta_sheet**: Used by [meta_read()]. Name of the sheet containing metadata (only required if the
#'   metadata is not on the first sheet of an `.xlsx` file).
#' - **meta_validate**: Used by [meta_read()]. Logical. If `TRUE`, checks the metadata for
#'   structural issues that could cause problems during processing.
#'   Recommended to keep `TRUE`.
#' - **iblank_pattern**: Used by [add_metadata()]. A character vector of length 1 with a regular expression that
#'   matches sample names of instrument blanks.
#' - **sblank_pattern**: Used by [add_metadata()]. A character vector of length 1 with a regular expression that
#'   matches sample names of sample blanks.
#' - **check_pattern**: Used by [add_metadata()]. A character vector of length 1 with a regular expression that
#'   matches sample names of check standards.
#' - **blank_validate**: Used by [add_blanks()]. Logical vector length one indicating whether blanks should be validated.
#' - **ex_clip**: Used by [process_eem()]. Numeric vector of length two specifying the minimum and
#'   maximum excitation wavelengths to keep.
#' - **em_clip**: Used by [process_eem()]. Numeric vector of length two specifying the minimum and
#'   maximum emission wavelengths to keep.
#' - **type**: Used by [process_eem()]. Logical vector of length four indicating which scattering lines to remove.
#'   The order is "raman1", "raman2", "rayleigh1", "rayleigh2".
#' - **width**: Used by [process_eem()]. Numeric vector of length four specifying the width of scattering
#'   lines to remove (nm). Same order as `type`.
#' - **interpolate**: Used by [process_eem()]. Logical vector of length four indicating which scattering
#'   lines to interpolate. Same order as `type`.
#' - **method**: Used by [process_eem()]. Numeric (0–4) specifying the interpolation method to use.
#'   Default is 1. See [staRdom::eem_interp()] for details.
#' - **cores**: Used by [process_eem()]. Integer specifying the number of cores for parallel computation
#'   during interpolation.
#' - **cuvle**: Used by [process_eem()]. Cuvette (path) length in cm.
#' - **index_method**: Used by [get_indices()]. Either "eemanalyzeR", "eemR", "usgs", or a custom
#'   function.
#' - **tolerance**: Used by [get_indices()]. Maximum percent deviation that the check standard can vary
#'   from the long-term values without being flagged.
#' - **return**: Used by [get_indices()].Output format: "long" or "wide".
#' - **qaqc_dir**: Used by [get_indices()]. File path to the QAQC files generated with [create_mdl()]
#'     and [create_std()]. Default is a user-specific data directory
#'     [rappdirs::user_data_dir()].
#' - **filename**: Used by [export_data()]. A character string, used for file names.
#' - **output_dir**: Used by [export_data()]. Path to save the data. Defaults to a temporary directory
#'   if not specified.
#' - **readme**: Starts as NULL, used to store notes and warnings about processing.

"default_config"
