# Perform inner-filter correction on EEM data

Wrapper around
[`eemR::eem_inner_filter_effect()`](https://rdrr.io/pkg/eemR/man/eem_inner_filter_effect.html)
that supports the augmented `eemlist` structure used in this package.
Unlike the base function, this version automatically trims EEM
wavelengths to match the available absorbance data instead of returning
an error.

## Usage

``` r
ife_correct(eemlist, abslist, cuvle = 1, arg_names = NULL)
```

## Source

Massicotte P. (2019). *eemR: Tools for Pre-Processing
Emission-Excitation-Matrix (EEM) Fluorescence Data.* R package version
1.0.1. <https://CRAN.R-project.org/package=eemR>

## Arguments

- eemlist:

  An `eemlist` object.

- abslist:

  An `abslist` object.

- cuvle:

  Cuvette (path) length in cm.

- arg_names:

  Optional list of arguments passed from higher-level functions for
  README generation.

## Value

An `eemlist` with inner-filter–corrected EEMs.

## Details

Inner-filter correction requires linking each EEM to its corresponding
absorbance spectrum. This function uses the metadata `name` field to
pair samples, so **metadata must be added** beforehand using
[`add_metadata()`](https://katiewampler.github.io/eemanalyzeR/reference/add_metadata.md).

## Examples

``` r
eemlist <- add_metadata(metadata, example_eems)
abslist <- add_metadata(metadata, example_abs)
correct_eem <- ife_correct(eemlist, abslist)
```
