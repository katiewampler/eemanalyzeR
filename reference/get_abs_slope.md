# Calculate spectral slopes

Computes spectral slope values using
[`staRdom::abs_fit_slope()`](https://rdrr.io/pkg/staRdom/man/abs_fit_slope.html),
including interpolation, absorption calculation, and QA/QC flagging for
missing wavelengths.

## Usage

``` r
get_abs_slope(abs, lim, cuvle = 1)
```

## Arguments

- abs:

  An `abs` or `abslist` object.

- lim:

  A numeric vector of length two giving the lower and upper wavelength
  limits used to calculate the slope.

- cuvle:

  Cuvette (path) length in cm.

## Value

A vector of spectral slope values, one per sample in the `abslist`. If
the slope cannot be calculated, the function returns `"DATA04"`.

## Examples

``` r
S275_295 <- get_abs_slope(example_abs, lim = c(275, 295))
```
