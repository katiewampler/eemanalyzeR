# Default package methods for fluorescence and absorbance indices

Calculates commonly used absorbance and fluorescence optical indices
from `eemlist` and `abslist`. For detailed descriptions and references
for indices, see the vignette
[eemanalyzeR-indices](https://katiewampler.github.io/eemanalyzeR/doc/eemanalyzeR-indices.md).

## Usage

``` r
eemanalyzeR_indices(eemlist, abslist, cuvle = 1, qaqc_dir = NULL)
```

## Arguments

- eemlist:

  An `eemlist` object.

- abslist:

  An `abslist` object.

- cuvle:

  Cuvette (path) length in cm.

- qaqc_dir:

  File path to the QAQC files generated with
  [`create_mdl()`](https://katiewampler.github.io/eemanalyzeR/reference/create_mdl.md)
  and
  [`create_std()`](https://katiewampler.github.io/eemanalyzeR/reference/create_std.md).
  Default is a user-specific data directory
  [`rappdirs::user_data_dir()`](https://rappdirs.r-lib.org/reference/user_data_dir.html).

## Value

A list with two elements:

- **eem_index**: a `data.frame` of all fluorescence indices. Each row
  corresponds to a single index for a sample.

- **abs_index**: a `data.frame` of all absorbance indices. Each row
  corresponds to a single index for a sample.

Each `data.frame` contains the following columns:

- **sample_name**: name of the sample from the EEM or absorbance list

- **meta_name**: sample name from metadata if provided; otherwise same
  as `sample_name`

- **index**: name of the index

- **value**: calculated value of the index

## Note

- If absorbance is not at a 1 nm interval, it will be interpolated using
  [`zoo::na.approx()`](https://rdrr.io/pkg/zoo/man/na.approx.html),
  which fills in missing values using linear interpolation.

- If EEM data is not at a 1 nm interval, fluorescence will be
  interpolated using
  [`pracma::interp2()`](https://rdrr.io/pkg/pracma/man/interp2.html).

## Examples

``` r
indices <- eemanalyzeR_indices(
  example_processed_eems,
  example_processed_abs,
  qaqc_dir = system.file("extdata", package = "eemanalyzeR")
)
```
