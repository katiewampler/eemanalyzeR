# Interpreting eemanalyzeR Outputs

## Exported Data

Every data export from `eemanalyzeR` using the
[`export_data()`](https://katiewampler.github.io/eemanalyzeR/reference/export_data.md)
or
[`run_eems()`](https://katiewampler.github.io/eemanalyzeR/reference/run_eems.md)
functions contains the following files:

- **\*\_indices_filename.csv**:The absorbance and fluorescence indices
- **absorbance_plot_filename.png:** Plot showing the absorbance spectra
- **\* .png:** Plots of the individual EEMs
- **summary_plots_filename.png:** Plot showing all of the EEMs together
- **processed_data_filename.rds:** An R readable object containing all
  of the exported data
- **readme_filename.txt:** A text file detailing the processing steps,
  any warnings that occurred during data processing, and information
  about the QA/QC checks performed

  

## QA/QC Checks

### Visual Validation of Instrument Blank

The instrument blank can be plotted and visually validated by the user.
This step is used to visually look for blank contamination or errant
peaks that could impact data during blank subtraction.

- If this step is performed the **readme** will contain a line that this
  visual check occurred and the user processing the data accepted the
  data.

- The
  [`validate_blanks()`](https://katiewampler.github.io/eemanalyzeR/reference/validate_blanks.md)
  function also has the option that if the user does **not** accept the
  instrument blank, it may be replaced with one of the analytical
  blanks. In this case the **readme** will note the blank that was used
  instead of the instrument blank.

  

### Method Detection Limits

Absorbance and fluorescence indices can be compared to method detection
limits (MDL). The MDL is the minimum signal at which we can confidently
distinguish a measurement from zero and from analytical blanks. Here,
the MDL is calculated using the approach proposed by Hansen et al.
(2018):

$$\text{MDL} = \text{mean}\left( \text{long-term blank} \right) + 3 \times \text{SD}\left( \text{long-term blank} \right)$$where
**long-term blank** is the fluorescence for a specific wavelength pair
or absorbance at a specific wavelength, **mean** is the average signal,
and **SD** is the standard deviation.

To use the MDL checks, the user must create MDL files for both
absorbance and fluorescence data using the
[`create_mdl()`](https://katiewampler.github.io/eemanalyzeR/reference/create_mdl.md)
function. If no MDL files are found, a warning will appear in the
**readme** file.

  

### Check Standards

Samples of sample type `check` can also be checked against a long-term
average for consistency.

To use the check standard checks, the user must create average standard
files for both absorbance and fluorescence data using the
[`create_std()`](https://katiewampler.github.io/eemanalyzeR/reference/create_std.md)
function.

- **If no standard files are found**: a warning will appear in the
  **readme** file.

- **If a standard file exists**: all available index values will be
  compared between the check standards within the run and the long term
  standard. If the value is outside the tolerance range (default is
  20%), it will be flagged in the index file. Additionally the
  **readme** file will contain a summary of the number of indices
  outside the tolerance range across the entire run and the tolerance
  used.

  

### Index QA/QC Flags

**The following flags may appear in the indices output files:**

- **DATA01**: Missing data required for calculation
- **DATA02**: Missing required wavelengths; value may be inaccurate
- **DATA03**: Ratio denominator was zero
- **DATA04**: Spectral slope could not be calculated
- **DOC01**: Missing dissolved organic carbon (DOC) data
- **INF01**: Infinite value
- **MDL01**: All values below MDL
- **MDL02**: One or more values below MDL; use cautiously
- **MDL03**: Ratio index where numerator or denominator was entirely
  below MDL
- **NEG01**: Negative value
- **STD01**: Check standard value outside tolerance
- **VAL01**: Value below typical range
- **VAL02**: Value above typical range

  

## Indices

The absorbance indices reported by the default index method
(`eemanalyzeR`) and their **typical** interpretations are:

| Index Name               | Index Abbreviation | Interpretation                              |
|:-------------------------|:-------------------|:--------------------------------------------|
| SUVA 254                 | SUVA254            | proxy for aromaticity                       |
| SUVA 280                 | SUVA280            | proxy for aromaticity                       |
| SVA 412                  | SVA412             | proxy for aromaticity                       |
| Spectral Slope (275-295) | S275_295           | related to molecular weight and aromaticity |
| Spectral Slope (350-400) | S350_400           | related to molecular weight and aromaticity |
| Spectral Slope Ratio     | SR                 | related to molecular weight                 |
| E₂/E₃                    | E2_E3              | related to molecular weight and aromaticity |
| E₄/E₆                    | E4_E6              | related to humic-like organic matter        |

  

The fluorescence indices reported by the default index method
(`eemanalyzeR`) and their **typical** interpretations are:

| Index Name                 | Index Abbreviation | Interpretation                                    |
|:---------------------------|:-------------------|:--------------------------------------------------|
| Peak B                     | pB                 | tyrosine-like, protein-like organic matter        |
| Peak T                     | pT                 | tryptophan-like, protein-like organic matter      |
| Peak A                     | pA                 | UV humic-like organic matter                      |
| Peak M                     | pM                 | marine humic-like organic matter                  |
| Peak C                     | pC                 | visible humic-like organic matter                 |
| Peak D                     | pD                 | soil fulvic acid-like organic matter              |
| Peak E                     | pE                 | soil fulvic acid-like organic matter              |
| Peak N                     | pN                 | related to phytoplankton productivity             |
| Ratio of Peak A to Peak T  | rAT                | ratio of humic-like to fresh organic matter       |
| Ratio of Peak C to Peak A  | rCA                | ratio of humic-like to fulvic-like organic matter |
| Ratio of Peak C to Peak M  | rCM                | amount of blueshifted organic matter              |
| Ratio of Peak C to Peak T  | rCT                | related to biochemical oxygen demand              |
| Fluorescence Index         | FI                 | terrestrial versus microbial sources              |
| Humification Index-Zsolnay | HIX                | indication of humic substances                    |
| Humification Index-Ohno    | HIX_ohno           | indication of humic substances                    |
| Freshness Index            | fresh              | indication of recently produced organic matter    |
| Biological Index           | BIX                | indicator of autotrophic activity                 |

  

For more details on interpretation and sources see the vignette
vignette(“eemanalyzeR-indices”).

  

## References

Hansen, A. M., Fleck, J., Kraus, T. E. C., Downing, B. D., von
Dessonneck, T., & Bergamaschi, B. (2018). *Procedures for using the
Horiba Scientific Aqualog® fluorometer to measure absorbance and
fluorescence from dissolved organic matter* (USGS Numbered Series No.
2018-1096). U.S. Geological Survey. <doi:10.3133/ofr20181096>
