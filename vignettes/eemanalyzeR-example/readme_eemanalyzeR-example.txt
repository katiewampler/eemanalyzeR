2026-04-15 15:34
Data processed using eemanalyzeR 1.1.0 package in R.
For details on processing steps, indices, and QA/QC flags see the package website: https://katiewampler.github.io/eemanalyzeR/articles/output-documentation.html
______________________________

2026-04-15 15:34:50: blanks were subtracted from data via 'subtract_blank' function

2026-04-15 15:34:50: scattering lines removed via 'remove_scattering' function
   function parameters:
	 type: c(TRUE, TRUE, TRUE, TRUE)
	 width: c(16, 3, 30, 10)
	 interpolate: c(TRUE, TRUE, FALSE, FALSE)
	 method: 1
	 cores: 1

2026-04-15 15:34:50: data was corrected for inner filter effects via 'ife_correct' function
   function parameters:
	 cuvle: 1
   warning: removed the following wavelengths in EEM's to match absorbance data wavelengths
	excitation: 
	emission: 806.452 - 820.768

2026-04-15 15:34:50: EEMs data was normalized for raman area via 'raman_normalize' function

2026-04-15 15:34:50: EEMs data was corrected for dilutions via 'correct_dilution' function

2026-04-15 15:34:50: Absorbance data was corrected for dilutions via 'correct_dilution' function

2026-04-15 15:34:50: EEMs data were cropped using the 'eemR::eem_cut' function
   function parameters:
	 ex_clip: c(247, 450)
	 em_clip: c(247, 600)

2026-04-15 15:34:52: Absorbance and fluorescence indices were calculated using the 'get_indices' function
   function parameters:
	 index_method: eemanalyzeR
	 return: wide
	 cuvle: 1
	 qaqc_dir: C:/Users/wampleka/Documents/Projects/eemanalyzeR/inst/extdata

2026-04-15 15:34:50: Fluorescence indices were checked against method detection limits (MDL) using method .
Absorbance indices were checked against method detection limits (MDL) using method .

2026-04-15 15:34:52: 0% (n=8) of the absorbance indices were greater than 20% of the long-term check standard
27% (n=22) of the fluorescence indices were greater than 20% of the long-term check standard

