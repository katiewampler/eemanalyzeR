# Interpolate absorbance data

Interpolates absorbance data to 1 nm resolution so indices can be
calculated at wavelengths that were not explicitly measured.

## Usage

``` r
abs_interp(abs, type = "linear")
```

## Arguments

- abs:

  An `abs` or `abslist` object.

- type:

  The interpolation method, either "linear" for linear interpolation
  using
  [`zoo::na.approx()`](https://rdrr.io/pkg/zoo/man/na.approx.html), or
  "spline" for spline interpolation using
  [`zoo::na.spline()`](https://rdrr.io/pkg/zoo/man/na.approx.html).

## Value

An `abs` or `abslist` object.

## Examples

``` r
abslist_filled <- abs_interp(example_abs)
abs_filled <- abs_interp(example_abs[[1]])
```
