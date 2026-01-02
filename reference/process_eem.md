# Process and correct excitation–emission matrices (EEMs)

Wrapper function that combines multiple EEM processing steps into a
streamlined workflow. The processing steps include:

- [`subtract_blank()`](https://katiewampler.github.io/eemanalyzeR/reference/subtract_blank.md):
  subtracts blank samples from the EEMs

- [`remove_scattering()`](https://katiewampler.github.io/eemanalyzeR/reference/remove_scattering.md):
  removes scattering lines with optional interpolation

- [`ife_correct()`](https://katiewampler.github.io/eemanalyzeR/reference/ife_correct.md):
  corrects for inner filter effects

- [`raman_normalize()`](https://katiewampler.github.io/eemanalyzeR/reference/raman_normalize.md):
  normalizes fluorescence intensity to Raman units

- [`correct_dilution()`](https://katiewampler.github.io/eemanalyzeR/reference/correct_dilution.md):
  adjusts intensity for sample dilutions

- [`eemR::eem_cut()`](https://rdrr.io/pkg/eemR/man/eem_cut.html): clips
  the excitation–emission matrices to the region of interest

## Usage

``` r
process_eem(
  eemlist,
  abslist,
  ex_clip = c(247, 450),
  em_clip = c(247, 600),
  type = c(TRUE, TRUE, TRUE, TRUE),
  width = c(16, 3, 30, 10),
  interpolate = c(TRUE, TRUE, FALSE, FALSE),
  method = 1,
  cores = 1,
  cuvle = 1
)
```

## Arguments

- eemlist:

  An `eemlist` object.

- abslist:

  An `abslist` object.

- ex_clip:

  Numeric vector of length two specifying the minimum and maximum
  excitation wavelengths to keep.

- em_clip:

  Numeric vector of length two specifying the minimum and maximum
  emission wavelengths to keep.

- type:

  Logical vector of length four indicating which scattering lines to
  remove. The order is "raman1", "raman2", "rayleigh1", "rayleigh2".

- width:

  Numeric vector of length four specifying the width of scattering lines
  to remove (nm). Same order as `type`.

- interpolate:

  Logical vector of length four indicating which scattering lines to
  interpolate. Same order as `type`.

- method:

  Numeric (0–4) specifying the interpolation method to use. Default
  is 1. See
  [`staRdom::eem_interp()`](https://rdrr.io/pkg/staRdom/man/eem_interp.html)
  for details.

- cores:

  Integer specifying the number of cores for parallel computation during
  interpolation.

- cuvle:

  Cuvette (path) length in cm.

## Value

An `eemlist` object with processed and corrected EEMs.

## Details

### Metadata

Absorbance and EEM data are linked via the `meta_name` in the metadata.
Metadata must already be added to the samples using
[`add_metadata()`](https://katiewampler.github.io/eemanalyzeR/reference/add_metadata.md).

### Tracking Processing Steps

Processing steps are tracked in two ways:

- Each sample has an attribute that records its processing steps. You
  can view this using `attributes(eem)`.

- Overall processing steps are recorded in a `readme` variable
  (accessible using
  [`print_readme()`](https://katiewampler.github.io/eemanalyzeR/reference/print_readme.md))
  or exported as the `readme.txt` file using
  [`export_data()`](https://katiewampler.github.io/eemanalyzeR/reference/export_data.md).

## Examples

``` r
eemlist <- add_metadata(metadata, example_eems)
abslist <- add_metadata(metadata, example_abs)
blanklist <- subset_type(eemlist, "iblank")
eemlist <- add_blanks(eemlist, blanklist)
corrected_eem <- process_eem(eemlist, abslist)
plot(corrected_eem)
```
