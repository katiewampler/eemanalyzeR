# Perform Raman normalization of EEMs

Normalizes the fluorescence intensity of EEMs to Raman Units (R.U.)
based on the area of the water Raman peak (Lawaetz and Stedmon, 2009).

## Usage

``` r
raman_normalize(eemlist)
```

## Source

Lawaetz, A. J., & Stedmon, C. A. (2009). Fluorescence Intensity
Calibration Using the Raman Scatter Peak of Water. Applied Spectroscopy,
63(8), 936-940.
[doi:10.1366/000370209788964548](https://doi.org/10.1366/000370209788964548)

## Arguments

- eemlist:

  An `eemlist` object.

## Value

An `eemlist` object with EEMs normalized to Raman Units.

## Details

The function uses the Raman peak area provided in the metadata to
normalize the EEMs. Therefore, metadata must be added to the samples
prior to normalization using
[`add_metadata()`](https://katiewampler.github.io/eemanalyzeR/reference/add_metadata.md).

## Examples

``` r
eemlist <- add_metadata(metadata, example_eems)
correct_eem <- raman_normalize(eemlist)
```
