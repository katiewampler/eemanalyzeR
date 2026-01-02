# Perform dilution corrections

Adjusts the EEM's and absorbance data fluorescence to account for
dilutions performed prior to measurement.

## Usage

``` r
correct_dilution(x)
```

## Arguments

- x:

  An `eemlist` or `abslist` object.

## Value

An object of the same class as `x` (`eemlist` or `abslist`) with
dilution corrections applied.

## Details

The function uses the dilution factor provided in the metadata to
correct the EEMs and absorbance values. Because of this, samples must
already have metadata added using
[`add_metadata()`](https://katiewampler.github.io/eemanalyzeR/reference/add_metadata.md).

## Examples

``` r
eemlist <- add_metadata(metadata, example_eems)
correct_eem <- correct_dilution(eemlist)

abslist <- add_metadata(metadata, example_abs)
correct_abs <- correct_dilution(abslist)
```
