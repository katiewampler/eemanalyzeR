# Visually validate the check standard

Visually inspect plots of the tea check standard absorbance compared to
the long-term standard.

## Usage

``` r
validate_std(abslist, qaqc_dir = NULL, tolerance = 0.2)
```

## Arguments

- abslist:

  An `abslist` object.

- qaqc_dir:

  File path to the QAQC files generated with
  [`create_mdl()`](https://katiewampler.github.io/eemanalyzeR/reference/create_mdl.md)
  and
  [`create_std()`](https://katiewampler.github.io/eemanalyzeR/reference/create_std.md).
  Default is a user-specific data directory
  [`rappdirs::user_data_dir()`](https://rappdirs.r-lib.org/reference/user_data_dir.html).

- tolerance:

  Maximum percent deviation that the check standard can vary from the
  long-term values without being flagged.

## Value

A `ggplot` object showing the absorbance of the check standards, the
long-term standard (dashed line), and the tolerance thresholds (gray
ribbon).

## See also

[`create_std()`](https://katiewampler.github.io/eemanalyzeR/reference/create_std.md)

## Examples

``` r
abslist <- add_metadata(metadata, example_abs)
validate_std(abslist, system.file("extdata", package = "eemanalyzeR"))
```
