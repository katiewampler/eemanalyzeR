# Get unique EEMs or absorbance samples

Removes duplicate EEM matrices or absorbance data from an `eemlist` or
`abslist`. Duplicates are determined based on the actual data (`eem$x`
or `abs$data`) rather than metadata or sample names. Only truly
duplicate EEM or absorbance data are removed.

## Usage

``` r
# S3 method for class 'eemlist'
unique(x, ...)

# S3 method for class 'abslist'
unique(x, ...)
```

## Arguments

- x:

  An `eemlist` or `abslist` object.

- ...:

  Additional arguments passed to
  [`base::unique()`](https://rdrr.io/r/base/unique.html).

## Value

An object of class `eemlist` or `abslist` with duplicate samples
removed.

## Details

The first duplicate will be retained. The sample name will not be
updated but will reflect the give name of the first duplicated sample.

## Examples

``` r
# Remove duplicate EEMs
unique_eems <- unique(example_eems)

# Remove duplicate absorbance data
unique_abs <- unique(example_abs)
```
