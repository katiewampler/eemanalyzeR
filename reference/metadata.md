# Example metadata

Metadata associated with the samples in
[`example_eems()`](https://katiewampler.github.io/eemanalyzeR/reference/example_eems.md)
and
[`example_abs()`](https://katiewampler.github.io/eemanalyzeR/reference/example_abs.md).
Provides an example of the structure and requirements for metadata
needed to run samples.

## Usage

``` r
metadata
```

## Format

A data.frame with 3 rows and 11 columns:

- **index**: Numeric order of entry (e.g., 1, 2, 3, etc.)

- **analysis_date**: Optional, date samples were run on instrument (not
  collected in the field)

- **description**: Optional, brief description of the sample collected

- **data_identifier**: REQUIRED, file name from the aqualog, must match
  exactly

- **replicate_no**: REQUIRED, number indicating if the sample was
  replicated; unreplicated = 1

- **integration_time_s**: REQUIRED, integration time of sample (e.g., 1,
  2, etc.)

- **dilution**: REQUIRED, dilution factor as decimal (e.g., 0.5 for
  2-fold dilution); 1 if no dilution

- **RSU_area_1s**: REQUIRED, RSU Adjust Area from the Raman test for
  normalization

- **sample_type**: Optional, flag (sample, sblank, or check)

- **run_type**: Optional, how the samples were run (manual or sampleQ)

- **collect_date**: Optional, date the water samples were collected

- **DOC_mg_L**: Optional, DOC concentration in mg/L of original sample

- **Notes**: Optional, any notes from collection or running

## Source

Oregon State University Forest Ecohydrology and Watershed Sciences Lab
(2022-11-14)
